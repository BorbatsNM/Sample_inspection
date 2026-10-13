# Функция построения приёмочной кривой из ГОСТ Р ИСО 3951-1 
# для одноступенчатого плана выборочного контроля по количественному
# признаку для s-метода. Аргументы функции:
#   n – объём выборки одноступенчатого плана в виде единственного целого 
#   числа (обязательный);
#   k – приёмочный коэффициент (контрольный норматив) в виде единственного
#   числа (обязательный);
#   limits – числовой двухкомпонентный список, первый элемент которого 
#   соответствует нижней границе допуска (спецификации),
#   а второй - верхней границе, является обязательным 
#   при необходимости построения кривой в масштабе реальных результатов
#   измерений и/или при наличии данных, передаваемых аргументу data;
#   data – числовой вектор, с результатами контроля по используемому плану,
#   является обязательным для необходимости построения приёмочной прямой
#   при наличии только одной границы допуска;
#   stdzn – логический аргумент, указывающий, нужно ли строить кривую
#   в стандартизованном масштабе (TRUE по умолчанию) или в масштабе
#   реальных результатов измерений (FALSE), в первом случае необходимо 
#   задание только аргументов n и k, а во втором – также аргумента limits, 
#   при построении приёмочной прямой в случае наличии только одной границы 
#   допуска, значение данного аргумента игнорируется;
#   na.rm – логический аргумент, если задано значение FALSE (по умолчанию)
#   отсутствующие значения в векторе данных data удаляются с предупреждением,
#   при значении TRUE пропущенные значения удаляются без предупреждения;
#   line_arg, point_arg – списки, используемые для пользовательской настройки 
#   отображения приёмочной кривой и точки, соответствующей результатам контроля
#   (при задании аргумента data) в виде задания пары «имя» – значение, 
#   где «имя» должно соответствовать названию графической эстетики, 
#   допустимой для применения в функциях geom_path() и geom_point() 
#   (см. vignette("ggplot2-specs"));
#   step – числовой аргумент, используемый для задания шага варьирования
#   значений уровня несоответствий p, используемых для построения приёмочной
#   кривой – чем меньше шаг, тем более плавная кривая получается, 
#   не рекомендуется увеличивать значение данного аргумента, 
#   так как для некоторых сочетании значений объёма выборки n 
#   и контрольного норматива k это может приводить к появлению графических
#   артефактов на изображении кривой или вообще не возможности её построения;
#   ratio – числовой аргумент, используемый для установления соотношения
#   масштабов по осям x и y графика, выраженное как y/x, по умолчанию 
#   используется значение 1, гарантирующее, что одна единица по оси x 
#   будет иметь ту же длину, что и одна единица по оси y, 
#   что соответствует представлению кривых в ГОСТ Р ИСО 3951-1. 

# --------------------------------------------------------------------------
# Подключение необходимых пакетов
# --------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(checkmate)


# ---------------------------------------------------------------------------
# Функция построения приёмочной кривой из ГОСТ Р ИСО 3951-1.
# Вспомогательная validate_ACurve_args() инкапсулирована через local()
# и не появляется в глобальном окружении.
# ---------------------------------------------------------------------------

ACurve <- local({
  
  # Внутрення функция для проверки корректности задания входных аргументов ####
  # Не предназначена для прямого вызова пользователем
  validate_ACurve_args <- function(n, k, limits, data, stdzn, na.rm,
                                   line_arg, point_arg, step, ratio) {
    if (!stdzn && is.null(limits)) {
      stop("Построение кривых в нестандартизованном масштабе невозможно без границ допуска!")
    }
    if (!is.null(data) && is.null(limits)) {
      stop("При наличии данных должны быть заданны и границы допуска.")
    }
    
    assert_int(n, .var.name = "n")
    if (n < 3) {
      stop("Аргумент 'n' должен быть не меньше 3: s-метод требует n - 2 > 0.")
    }
    
    assert_number(k, finite = TRUE, .var.name = "k")
    if (k <= 0) {
      stop("Аргумент 'k' должен быть положительным.")
    }
    if (k > (n - 1)/sqrt(n)) {
      stop(sprintf(
        "При n = %d значение аргумента 'k' не должно превышать (n - 1)/sqrt(n) = %.4f, иначе формулы вернут NaN.",
        n, (n - 1)/sqrt(n)
      ))
    }
    
    assert_flag(stdzn, .var.name = "stdzn")
    assert_flag(na.rm, .var.name = "na.rm")
    
    assert_number(step, finite = TRUE, .var.name = "step")
    if (step <= 0 || step >= 1) {
      stop("Аргумент 'step' должен быть числом из интервала (0, 1).")
    }
    if (step < 1e-6) {
      warning("Очень малое значение аргумента 'step' может привести к зависанию.")
    }
    
    assert_number(ratio, finite = TRUE, .var.name = "ratio")
    if (ratio <= 0) {
      stop("Аргумент 'ratio' должен быть положительным конечным числом.")
    }
    
    if (!is.null(limits)) {
      assert_list(limits, len = 2, .var.name = "limits")
      for (i in 1:2) {
        v <- limits[[i]]
        if (!is.null(v)) {
          assert_number(v, finite = TRUE, .var.name = paste0("limits[[", i, "]]"))
        }
      }
      L <- limits[[1]]
      U <- limits[[2]]
      if (is.null(L) && is.null(U)) {
        stop(paste(
          "Хотя бы одна граница допуска должна быть задана.\n",
          "Если необходимо построить приёмочную кривую в общем виде в стандартизованном масштабе,\n",
          "используйте 'limits = NULL' и 'stdzn = TRUE'.\n"
        ))
      }
      if (!is.null(L) && !is.null(U) && L >= U) {
        stop("Нижняя граница допуска должна быть строго меньше верхней.")
      }
    }
    
    if (!is.null(data)) {
      assert_numeric(data, min.len = 1, any.missing = TRUE, finite = TRUE)
      if (length(data) != n) {
        warning(sprintf(
          "Число значений в 'data' (%d) не совпадает с объёмом выборки n (%d)",
          length(data), n
        ))
      }
      n_eff <- sum(!is.na(data))
      if (n_eff < 3) {
        stop("После удаления пропусков в 'data' останется менее 3 наблюдений.")
      }
      if (sd(data, na.rm = TRUE) < .Machine$double.eps^0.5) {
        stop("Стандартное отклонение выборки равно нулю!")
      }
    }
    
    assert_list(line_arg, .var.name = "line_arg")
    assert_list(point_arg, .var.name = "point_arg")
    for (nm in c("line_arg", "point_arg")) {
      obj <- get(nm)
      if (length(obj) > 0) {
        if (is.null(names(obj)) || any(names(obj) == "")) {
          stop(sprintf("Все эелементы '%s' должны быть именованными.", nm))
        }
      }
    }
    reserved_line  <- intersect(names(line_arg),  c("data", "mapping", "x", "y"))
    reserved_point <- intersect(names(point_arg), c("geom", "x", "y"))
    if (length(reserved_line)) {
      warning("В 'line_arg' игнорируются/переписываются имена: ",
              paste(reserved_line, collapse = ", "))
    }
    if (length(reserved_point)) {
      warning("В 'point_arg' игнорируются/переписываются имена: ",
              paste(reserved_point, collapse = ", "))
    }
    invisible(TRUE)
  }
  
  
  # --- Функция построения приёмочной кривой из ГОСТ Р ИСО 3951-1 ####
  function(n, k, limits = NULL, data = NULL, 
           stdzn = TRUE, na.rm = FALSE,
           line_arg = list(), point_arg = list(),
           step = 0.0001, ratio = 1) {
    
    # вызов внутренней функции для проверки аргументов
    validate_ACurve_args(n, k, limits, data, stdzn, na.rm,
                         line_arg, point_arg, step, ratio)
    
    # графические эстетики для построения линии по умолчанию
    if (!"linewidth" %in% names(line_arg)) {
      line_arg$linewidth  <-  1
    }
    
    # вычисление статистик по данным, если они переданы
    if (!is.null(data)) {
      avg <- mean(data, na.rm = TRUE)
      s <- sd(data, na.rm = TRUE)
      x_min <- min(data, na.rm = TRUE)
      x_max <- max(data, na.rm = TRUE)
      if (any(is.na(data)) && !na.rm) {
        warning(paste0("Удалены отсутствующие значения.", "\n",
                       "Число фактических наблюдений меньше объёма выборки!"))
      }
      # графические эстетики для нанесения точки по умолчанию
      if (!"size" %in% names(point_arg)) {
        point_arg$size <- 3
      }
    }
    
    if (any(sapply(limits, is.null)) && !is.null(data)) {
      # построение приёмочных прямых при наличии только одной границы допуска
      if (is.null(limits[[1]])) {
        U <- limits[[2]]
        S <- seq(0, 1/k*(x_min), length.out = 100)
        X <- U - k*S
        tb <- tibble(x = S, y = X)
        tb <- tb[X >= x_min, ]
      } else {
        L <- limits[[1]]
        S <- seq(0, 1/k*(x_max), length.out = 100)
        X <- L + k*S
        tb <- tibble(x = S, y = X)
        tb <- tb[X <= x_max, ]
      }
      
      # дополнительные аргументы для нанесения точки
      point_arg$geom <- "point"
      point_arg$x <- s
      point_arg$y <- avg
      
      # построение графика
      ggplot(tb, aes(x, y)) +
        do.call(geom_line, line_arg) +
        do.call(annotate, point_arg) +
        labs(x = "s", y = expression(bar(x))) +
        coord_cartesian(expand = FALSE)
      
    } else { # построение приёмочных кривых для двух границ допуска
      
      # критическое значение уровня несоответствий
      M <- pbeta(0.5*(1 - k*sqrt(n)/(n - 1)), (n - 2)/2, (n - 2)/2)
      
      if (stdzn) {
        # тиббл с координатами вершины кривой
        tb1 <- tibble(p01 = seq(0, M, by = step)) |> 
          mutate(p02 = M - p01) |> 
          mutate(xN = (1 - 2*qbeta(p01, (n - 2)/2, (n - 2)/2)) /
                   (2*(1 - qbeta(p01, (n - 2)/2, (n - 2)/2) - 
                         qbeta(p02, (n - 2)/2, (n - 2)/2)))) |> 
          mutate(sN = 1/(2*(1 - qbeta(p01, (n - 2)/2, (n - 2)/2) - 
                              qbeta(p02, (n - 2)/2, (n - 2)/2))) * 
                   (sqrt(n)/(n - 1)))
        
        # конечное значение СТО для короткой ветви вершины кривой
        if (n == 3) {
          sN_lim_1 <- tb1 |> 
            filter(xN < 0.5) |> 
            pull(sN) |> 
            max()
          sN_lim_2 <- tb1 |> 
            filter(xN > 0.5) |> 
            pull(sN) |> 
            max()
          sN_lim <- min(sN_lim_1, sN_lim_2)
        } else {
          sN_lim_1 <- tb1 |> 
            filter(xN < 0.5) |> 
            pull(sN) |> 
            min()
          sN_lim_2 <- tb1 |> 
            filter(xN > 0.5) |> 
            pull(sN) |> 
            min()
          sN_lim <- max(sN_lim_1, sN_lim_2)
        }
        
        # тиббл с координатами ветвей кривой
        tb2 <- tibble(xN = seq(0, 1, step),
                      sN = ifelse(xN < 0.5, xN/k, -xN/k + 1/k)) |> 
          mutate(sN = ifelse(sN <= sN_lim, sN, NA))
        
        # другие переменные для построения графика
        breaks  <-  seq(0, 1, by = 0.1)
        xname <-  expression(paste("(", bar(x), "- L) / (U - L)"))
        Sname <- "s / (U - L)"
        
        if (!is.null(data)) {
          # стандартизованные выборочные средние и стандартное отклонение
          avg_std <- (avg - limits[[1]])/(limits[[2]] - limits[[1]])
          s_std <- s/(limits[[2]] - limits[[1]])
          # дополнительные аргументы для нанесения точки
          point_arg$geom <- "point"
          point_arg$x <- s_std
          point_arg$y <- avg_std
        }
        
      } else {
        L <- limits[[1]]
        U <- limits[[2]]
        
        tb1 <- tibble(p01 = seq(0, M, by = step)) |> 
          mutate(p02 = M - p01) |> 
          mutate(xN = (U*(1 - 2*qbeta(p01, (n - 2)/2, (n - 2)/2)) - 
                         L*(2*qbeta(p02, (n - 2)/2, (n - 2)/2) - 1)) /
                   (2*(1 - qbeta(p01, (n - 2)/2, (n - 2)/2) - 
                         qbeta(p02, (n - 2)/2, (n - 2)/2)))) |> 
          mutate(sN = (U - L)/(2*(1 - qbeta(p01, (n - 2)/2, (n - 2)/2) - 
                                    qbeta(p02, (n - 2)/2, (n - 2)/2))) * 
                   (sqrt(n)/(n - 1)))
        
        if (n == 3) {
          sN_lim_1 <- tb1 |> 
            filter(xN < 0.5*(L + U)) |> 
            pull(sN) |> 
            max()
          sN_lim_2 <- tb1 |> 
            filter(xN > 0.5*(L + U)) |> 
            pull(sN) |> 
            max()
          sN_lim <- min(sN_lim_1, sN_lim_2)
        } else {
          sN_lim_1 <- tb1 |> 
            filter(xN < 0.5*(L + U)) |> 
            pull(sN) |> 
            min()
          sN_lim_2 <- tb1 |> 
            filter(xN > 0.5*(L + U)) |> 
            pull(sN) |> 
            min()
          sN_lim <- max(sN_lim_1, sN_lim_2)
        }
        
        tb2 <- tibble(xN = seq(L, U, (U - L)*step),
                      sN = ifelse(xN < 0.5*(L + U), xN/k - L/k, -xN/k + U/k)) |> 
          mutate(sN = ifelse(sN <= sN_lim, sN, NA))
        
        breaks  <-  seq(L, U, by = 0.1*(U - L))
        xname <-  expression(bar(x))
        Sname <- "s"
        
        if (!is.null(data)) {
          point_arg$geom <- "point"
          point_arg$x <- s
          point_arg$y <- avg
        }
      }
      
      # дополнительные аргументы для построения кривой
      line_arg2 <- line_arg
      line_arg2$data <- tb2
      line_arg2$na.rm <- TRUE
      
      # построение кривой
      fig <- ggplot(data = tb1, aes(x = sN, y = xN)) +
        do.call(geom_path, line_arg2) +
        do.call(geom_path, line_arg) +
        scale_y_continuous(breaks = breaks, 
                           expand = expansion(0)) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) + 
        labs(x = Sname, y = xname) +
        coord_fixed(ratio = ratio)
      
      if (!is.null(data)) {
        fig + do.call(annotate, point_arg)
      } else {
        fig
      }
    }
  }
})
