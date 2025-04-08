#' Select layers from a SpatRaster object
#'
#' @description
#' This function allows you to select specific layers from a SpatRaster object
#' using tidyverse-style syntax similar to dplyr's select. Supports selection
#' by name, position, and tidyselect helpers.
#'
#' @param .data A SpatRaster object containing multiple layers
#' @param ... Expressions for selecting layers. You can use:
#'   - Layer names: `select(r, layer1, layer2)`
#'   - Numeric indices: `select(r, 1, 3)`
#'   - Negative selection: `select(r, -layer1)` (exclude layer1)
#'   - tidyselect helpers: `select(r, starts_with("temp"))`
#'
#' @return A SpatRaster object containing only the selected layers
#'
#' @examples
#' \dontrun{
#' library(terra)
#' r <- rast(c("elevation.tif", "slope.tif", "aspect.tif"))
#'
#' # Select by name
#' r_elev <- select(r, elevation)
#'
#' # Select multiple layers
#' r_sub <- select(r, elevation, slope)
#'
#' # Exclude layers with negative selection
#' r_no_aspect <- select(r, -aspect)
#'
#' # Use tidyselect helpers
#' r_s <- select(r, starts_with("s"))
#' }
#' 
#' @importFrom rlang enquos expr quo_get_expr
#' @importFrom tidyselect eval_select
#' @importFrom terra nlyr
#' @export
select.SpatRaster <- function(.data, ...) {
  # 检查输入类型
  if (!inherits(.data, "SpatRaster")) {
    stop("Input must be a SpatRaster object")
  }

  # 获取图层名称
  layer_names <- names(.data)
  n_layers <- terra::nlyr(.data)

  # 如果没有提供选择条件，返回原始数据
  if (missing(...)) {
    return(.data)
  }

  # 捕获表达式
  dots <- rlang::enquos(...)

  # 使用tidyselect处理选择表达式
  # 创建一个模拟数据框，列名与栅格层名相同
  df_proxy <- stats::setNames(
    data.frame(matrix(0, nrow = 1, ncol = n_layers)),
    layer_names
  )

  # 评估选择表达式，获取选择列的位置
  pos <- tidyselect::eval_select(rlang::expr(c(...)), df_proxy)

  # 如果是负值选择，转换为对应的正值位置
  if (length(pos) == 0 && any(sapply(dots, function(x) {
    rlang::quo_get_expr(x) %>%
      as.character() %>%
      grepl("^-", .)
  }))) {
    # 处理负号选择
    all_pos <- seq_len(n_layers)
    pos <- setdiff(all_pos, abs(pos))
  }

  # 返回选择的图层
  if (length(pos) == 0) {
    warning("No layers selected")
    return(NULL)
  } else {
    return(.data[[pos]])
  }
}
