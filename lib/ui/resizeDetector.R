initialiseDetectHeightJS <- function() {
    shinyjs::extendShinyjs(text = "
    // Debounce function to prevent multiple calls to the same function
    debounce = function(fn, wait, immediate = false) {
      let timer;
      return function (...args) {
        const callNow = immediate && !timer;
        clearTimeout(timer);
        timer = setTimeout(function() {
          timer = null;
          if (!immediate) fn.apply(this, args);
        }, wait);
        if (callNow) fn.apply(this, args);
      };
    };

    // sendResize sends the size of the plot to Shiny
    sendResize = function(plotId) {
      let plotEl = document.getElementById(plotId);
      if (plotEl && plotEl.checkVisibility && plotEl.checkVisibility()) {
        let bb = plotEl.getBoundingClientRect();
        let wh = window.innerHeight;
        Shiny.onInputChange(plotId + 'Size', wh - bb.top);
      } else {
        Shiny.onInputChange(plotId + 'Size', 400);
      }
    };

    // registerResize adds event listeners to the window and to the document to handle tab selection and resize
    registerResize = function(tabName, plotId) {
      window.addEventListener('resize', debounce(function() {
        sendResize(plotId);
      }, 100));

      $(document).on('shiny:connected', function() {
        sendResize(plotId);

        $('a[data-toggle=\"tab\"][data-value=\"' + tabName + '\"]').on('shown.bs.tab', function (e) {
          sendResize(plotId);
        });
      });
    };
    ", functions = c())
}


detectHeightJS <- function(tabID, plotID) {
    shinyjs::extendShinyjs(text = paste("registerResize('", tabID, "', '", plotID, "');", sep=""), functions = c())
}

detectedHeight <- function(input, plotID, defaultHeight = 400, border = 20) {
    height <- as.numeric(input[[paste(plotID, "Size", sep = "")]]) - border
    if (height < defaultHeight) {
        height <- defaultHeight
    }
    height
}