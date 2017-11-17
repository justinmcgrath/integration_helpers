autoplot = function(func, plot_func) {
plot_id = NULL

    ensure_device = function() {
        if (is.null(plot_id) || !plot_id %in% dev.list()) {
            x11()
            plot_id <<- dev.cur()
        } else if (dev.cur() != plot_id) {
            dev.set(plot_id)
        }
    }

    update = function(df) {
        ensure_device()
        dev.hold()
        plot_func(df)
        dev.flush()
    }

    function(...) {
       result = func(...)
       update(result)
       result
   } 
}

