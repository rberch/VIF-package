#' -Variance Inflation Factor (VIF)-
#'
#' An R package that computes the Variance Inflation Factor (VIF) of predictor variables
#' @return A list.
#' @param Data a dataframe or matrix of predictors
#' @param multi logical stating whether to drop the variable with VIF greater than maxim, and proceed to recompute VIF until no multicollinearity exist based on maxim value defined. Default is FALSE, which just computes the VIF of the current data
#' @param  maxim numeric value. The maximum acceptable VIF value. Predictor variables are sequentially kicked out of the model if they have VIF greater than the maxim when multi = TRUE. Default is value 4.
#' @examples
#' # Using the Boston data from MASS package
#' df <- data.frame(MASS::Boston[,-1])
#' # When Interested in the VIF of the current data (multi = FALSE, as in default)
#' xs <- VIFsM(df, multi=FALSE,maxim = 4)
#' Plot(x = xs,maxx = 4)
#' summarY(object=xs)
#' # When Interested in checking the VIF of the data and dropping the variable with VIF > maxim
#' xs <- VIFsM(df, multi=TRUE,maxim = 4)
#' Plot(x = xs,multi = TRUE, maxx = 4)
#' summarY(object=xs, multi = TRUE)
#' @export


VIFsM <- function(Data, multi=FALSE,maxim = 4){   # argument is a matrix of the predictor variables

  VIF <- NULL

  if(!multi){

    n <- ncol(Data)

    VIF.out <- sapply(1:n, \(j) {
      work.frame <- data.table::data.table(y = Data[,j], x=Data[,-j])

      R2.temp<-summary(stats::lm(y ~ ., data=work.frame))$r.squared # regress predictor j on the
      # other predictors and extract R^2

      return(1/(1-R2.temp))
    })

    outss <- data.table::data.table(Predictor = colnames(Data), VIF=VIF.out) |>
      dplyr::arrange(dplyr::desc(VIF))

    structure(list(outss = outss), class = "VIFs")
  }else{

    Predictor <- NULL

    listy <- list()

    max.vif <- 10^8

    while(max.vif > maxim){

      n <- ncol(Data)

      VIF.out <- sapply(1:n, \(j) {
        work.frame <- data.table::data.table(y = Data[,j], x=Data[,-j])

        R2.temp<-summary(stats::lm(y ~ ., data=work.frame))$r.squared # regress predictor j on the
        # other predictors and extract R^2

        return(1/(1-R2.temp))
      })

      outss <- data.table::data.table(Predictor = colnames(Data), VIF=VIF.out) |>
        dplyr::arrange(dplyr::desc(VIF))

      if(max(VIF.out) > maxim){

        #outsDFl <- outsDF$outss[min(which(outsDF$outss$VIF > maxim)),Predictor]
        outsDFl <- names(Data)[which.max(VIF.out)]
        KO_var_number <- which.max(VIF.out)

        Data <- Data[,-KO_var_number]

        max.vif <- max(VIF.out)

        listy <- c(listy,list(outss, kicked_OUT = outsDFl))

      }else{max.vif = max(VIF.out)
      listy <- c(listy,list(outss, kicked_OUT = "All VIF values are less than maxim"))}


    }

    structure(listy, class = "VIFsM")

  }

}




#' @rdname VIFsM
#' @return a plot of the VIF values
#' @param x object of class \code{VIFsM}
#' @param maxx numeric value. The maximum acceptable VIF value. Predictor variables are sequentially kicked out of the model if they have VIF greater than the maxim when multi = TRUE. Default is value 4.
#' @param multi logical stating whether to drop the variable with VIF greater than maxim, and proceed to recompute VIF until no multicollinearity exist based on maxim value defined. Default is FALSE, which just computes the VIF of the current data
#' @param ... Further arguments passed to ggplot2
#'
#' @export
Plot <- function(x, maxx = 4,multi = FALSE,...){

  VIF <- NULL

  Predictor <- NULL

  df <- x$outss #|> data.frame()


  if(!multi){

    df <- x$outss #|> data.frame()

    ylimm <- c(max(x$outss$VIF))

    p1 <- ggplot2::ggplot(df,ggplot2::aes(x = stats::reorder(Predictor,+VIF),y = VIF)) +
      ggplot2::geom_col(width = 0.5, fill = "steelblue") +
      ggplot2::xlab("Variable") +
      ggplot2::geom_hline(yintercept = maxx, linetype="dashed", color = "red") +
      ggplot2::scale_y_continuous(limits = c(0,ylimm + 1), breaks = seq(0,ylimm + 1, by = 1)) +
      ggplot2::ylab("VIF") +
      ggplot2::coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("VIFs of Predictor variables")

    p1

  }else{

    out_num <- length(x)

    plotlist <- list()

    df1 <- x[[1]] # extract to use for maintaining the same scale on y-axis

    ylm <- max(df1$VIF) # y-axis max

    for (i in 1:out_num) {
      if (i %% 2 ==1) {
        df <- x[[i]]

        p1 <- ggplot2::ggplot(df,ggplot2::aes(x = stats::reorder(Predictor,+VIF),y = VIF)) +
          ggplot2::geom_col(width = 0.5, fill = "steelblue") +
          ggplot2::xlab("Variable") +
          ggplot2::geom_hline(yintercept = maxx, linetype="dashed", color = "red") +
          ggplot2::scale_y_continuous(limits = c(0,ylm + 1), breaks = seq(0,ylm + 1, by = 1)) +
          ggplot2::ylab("VIF") +
          ggplot2::coord_flip() +
          ggplot2::theme_bw() +
          ggplot2::ggtitle("VIFs of Predictor variables")

        plotlist[[i]] <- p1

      }

    }

    gridExtra::grid.arrange(grobs=plotlist,nrow=out_num/2)

  }



}




#' @rdname VIFsM
#' @return a summary of the VIF values
#' @param object of class \code{VIFsM}
#' @param multi logical stating whether to drop the variable with VIF greater than maxim, and proceed to recompute VIF until no multicollinearity exist based on maxim value defined. Default is FALSE, which just computes the VIF of the current data
#'
#' @export
summarY <- function(object,multi = FALSE){

  if(!multi){

    df <- object$outss

    print(df)

  }else{

    out_num <- length(object)

    summList <- list()

    koList <- list()

    j=1

    for (i in 1:out_num) {

      if (i %% 2 ==1) {

        df <- object[[i]]
        summList[[j]] <- df




        KO <- object[[i+1]]

        koList[[j]] <- KO

        j <- j+1

      }

    }

    print(list("VIF summaries" = summList, "Variable kicked OUT due to large VIF value" = koList))

  }



}
