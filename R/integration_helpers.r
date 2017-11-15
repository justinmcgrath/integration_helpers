# Given a data frame with r rows and c columns, one column being time, and a desired time, t, return a vector of c elements, one for each column, of values interpolated between values in the two rows whose times are an interval that contains t.
#
# v = interpolate(df, t)

# Given a data frame, and an interpolation method, return an interpolation function.
# interpolate(df, t) = interpolator(df, method)

interpolater = function(df, time_name, method) {
    n_col = ncol(df)
    times = df[[time_name]]
    func_list = vector('list', n_col - 1)
    not_time_columns = setdiff(names(df), time_name)

    for (col_name in not_time_columns) {
        func_list[[col_name]] = method(times, df[[col_name]])
    }

    result = vector('list', n_col)
    names(result) = names(df)

    interpolate = function(t) {
        result[[time_name]] <<- t
        for (col_name in not_time_columns) {
            result[[col_name]] <<- func_list[[col_name]](t)
        }
        return(result)
    }

    rm(df, times, n_col, method, col_name)
    return(interpolate)
}

time_reporter = function() {
    counter_ = 0L
    last_update_ = counter_
    iter_size_ = 1e6
    iter_times_ = numeric(iter_size_)
    is.na(iter_times_) = TRUE

    pb = winProgressBar("Progress", label=0)
    gc()  # Force stray progress bar windows to close.

    reset = function() {
        counter_ <<- 0
        last_update_ <<- counter_
        iter_times_ <<- numeric(iter_size_)
        is.na(iter_times_) <<- TRUE
        setWinProgressBar(pb, value=0, label=0)
        invisible()
    }

    update = function(state, t) {
        counter_ <<- counter_ + 1
        iter_times_[counter_] <<- t
        if (counter_ - last_update_ > 51) {
            setWinProgressBar(pb, value=0, label=sprintf("count: %d, time: %0.2f\n", counter_, t))
            last_update_ <<- counter_
        }
        invisible()
    }

    counter = function() { counter_ }

    iter_times = function () { iter_times_[!is.na(iter_times_)] }

    return(list(counter=counter, iter_times=iter_times, update=update, reset=reset))
}

state_reporter = function() {
    state_df = list()
    times = numeric()
    state_index = 0L
    highest_index = 0L
    state_ = NULL
    updated = FALSE
    rows = 2^20

    reset = function() {
        state_df <<- list()
        times <<- numeric()
        state_index <<- 0L
        highest_index <<- 0L
        state_ <<- NULL
        updated <<- FALSE
        rows <<- 1e5
        invisible()
    }

    update = function(state, time) {
        updated <<- TRUE
        if (length(state_df) == 0L) {
            state_df <<- vector('list', rows)
            times <<- numeric(rows)
        }
        valid_times = times[seq_len(state_index)]
        if (suppressWarnings(max(valid_times, na.rm=TRUE)) < time) {
            state_index <<- state_index + 1L
        } else {
            state_index <<- as.integer(min(which(valid_times >= time), na.rm=TRUE))
        }
        if (state_index > highest_index) {
            highest_index <<- state_index
            if (state_index > rows) {
                copy = state_df
                rows <<- rows * 2
                state_df <<- vector('list', rows)
                state_df[seq_len(state_index - 1)] <<- copy
            }
        }
        state_df[[state_index]] <<- unlist(state)
        times[state_index] <<- time
    }

    state = function() {
        if (updated) {
            state_df[seq(state_index + 1L, highest_index + 1L)] <<- list(NULL)
            state_ <<- as.data.frame(do.call(rbind, state_df[!is.na(state_df)]))
            updated <<- FALSE
        }
        return (state_)
    }

    return(list(reset=reset, update=update, state=state))
}

combine_reporters = function(x) {
    stopifnot(is.list(x))
    stopifnot(is.function(x[[1]][[1]]))
    functions = list()

    for (reporter in x) {
        for (func_name in names(reporter)) {
            functions[[func_name]][length(functions[[func_name]]) + 1] = reporter[func_name]
        }
    }

    caller = function(fn) {
        fn = fn  # Force evaluation of fn so that each caller has its own copy.
        function(...) {
            result = list()
            for (func in functions[[fn]]) result[[length(result) + 1]] = do.call(func, list(...))

            if (length(result) == 0)
                invisible()
            else if (length(result) == 1)
                return(result[[1]])
            else
                return(result)
        }
    }

    func_calls = list()
    for (func_name in names(functions)) {
        func_calls[[func_name]] = caller(func_name)
    }

    return (func_calls)
}

