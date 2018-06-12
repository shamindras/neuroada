#-------------------------------------------------------------------------------
# Core SPECTROGRAM functions
#-------------------------------------------------------------------------------

#' Creates a spectrogram from the univariate time series x
#' This function is created using code from Peter Elliot (CMU Statistics)
#' @param inp_trial
#' @param is the sampling rate (samples per second)
#' @param n is the number of time points to use per Fourier transform
#' @param overlap overlap controls the jump from bin to bin the time resolution
#' of the spectrogram is (n - overlap)
#' @return s is a list with three values
#'             s$t is a vector of times associated with the beginning of each bin
#'             s$f is a vector of frequencies that are measured for each bin.
#'                 s$f spans 0 to Fs/2 and has length n/2
#'             s$S is a matrix of intensities - Frequency x Time
#' @export
erp_create_specgram_trial <- function(inp_trial, Fs = 1000,
                                      n = 128, overlap = 96){
    sgram <- signal::specgram(x = inp_trial, Fs = Fs,
                              n = n, overlap = overlap)
    base::return(sgram)
}


#' Extracts the matrix of complex valued spectrogram entries
#'
#' @param inp_sgram The input spectrogram object as created from the
#' \code{signal::specgram} function
#'
#' @return a matrix of complex value spectrogram entries from the input
#' spectrogram object
#' @export
extract_specgram_mat <- function(inp_sgram){
    base::return(inp_sgram$S)
}


#' Create and store the spectrogram plot using a customized `copper` color
#' palette
#' This function is created using code from Peter Elliot (CMU Statistics)
#'
#' @param inp_sgram The input spectrogram object as created from the
#' \code{signal::specgram} function
#'
#' @return
#' @export
erp_plot_specgram_trial <- function(inp_sgram){

    # custom color scheme for displaying the spectrogram
    # low intensity --> black
    # high intensity --> goldenrod
    # bias controls spacing for high intensities (higher bias, more spacing)
    # copper is now a function that takes a number and returns a
    # vector of colors following the prescribed gradient
    copper <- grDevices::colorRampPalette(c("black", "navy blue", "goldenrod"),
                                          bias=2)

    # plots the spectrogram with restricted frequencies using the
    # specified colors
    plt_pryr %<a-% {
        graphics::plot(inp_sgram, col = copper(512), ylab = "Frequency")
        title("ERP Spectrogram")
        # adds a line marking stimulus onset (t = 500ms)
        abline(v=0.5, col="yellow", lwd=2)
    }

    base::return(plt_pryr)
}
