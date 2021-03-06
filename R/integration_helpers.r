# When integrating, the state at an arbitraty time must be known, but typically data are available at discontinuous times.
# R provides functions to map a discontinuous function to a continuous one, e.g., approxfun(), but does not provide similar functions for
# data frames. This function provides an interpolater that, given a discrete function of x and an interpolation method, returns a continuous function of x.

# Given a data frame, and an interpolation method, return an interpolation function.
# continuous_df = interpolater(df, x_name, method)
# df: the data to interpolate (a data frame).
# x_name: the name of the column in df that will be the input of the continuous function (a character vector).
# method: a function of the form f(x, y), that returns g(x) where g is a continuous function of x (a function).

# Example:
# df = data.frame(x = c(1, 2, 3), y1 = c(2, 5, 8), y2=c(3, 8, 5))
# piecewise_linear_df = interpolater(df, 'x', approxfun)
# piecewise_linear_df(1.5)
#
# To get a data.frame of many values use something like the following:
# as.data.frame(piecewise_linear_df(seq(1, 3, length=10)))

interpolater = function(df, x_name, method) {
    n_col = ncol(df)
    times = df[[x_name]]
    func_list = vector('list', n_col - 1)
    not_time_columns = setdiff(names(df), x_name)

    for (col_name in not_time_columns) {
        if (sum(!is.na(df[[col_name]])) >= 2) {
            func_list[[col_name]] = method(times, df[[col_name]])
        } else {
            func_list[[col_name]] = function(x) return(NA)
        }
    }

    result = vector('list', n_col)
    names(result) = names(df)

    interpolate = function(t) {
        result[[x_name]] <<- t
        for (col_name in not_time_columns) {
            result[[col_name]] <<- func_list[[col_name]](t)
        }
        return(result)
    }

    rm(df, times, n_col, method, col_name)
    return(interpolate)
}


# It's common to minimize sum of squares when optimizing.
# If using multiple dependent variables, the sums of squares should be normalized by the predicted value, which is chi-squared distributed.
# This function returns the sum of the chi values for the difference between observed and predicted values.
# observed_df: the observed data - a data frame.
# x_name: the name of the independent variable.
# predicted_df: the predicted data - data frame.
# The sum of squares is calculated for columns with the same names in observed_df and predicted_df, except for the x_name column.

chi_difference = function(observed_df, x_name, predicted_df) {
        if (!x_name %in% names(observed_df)) stop(paste(x_name, 'must be in observed_df'))
        if (!x_name %in% names(predicted_df)) stop(paste(x_name, 'must be in predicted'))
        y_names = setdiff(intersect(names(observed_df), names(predicted_df)), x_name)
        i_result = interpolater(predicted_df[c(y_names, x_name)], x_name, approxfun)

        ss = 0
        for (y in y_names) {
            pred_y = i_result(observed_df[[x_name]])[[y]]  # Predict y at all observed times.

            obs_y = observed_df[[y]] 
            undefined_ind = pred_y == 0 & obs_y == 0
            ss_y = abs(sum(((obs_y - pred_y)^2 / pred_y)[!undefined_ind], na.rm=TRUE))  # Chi squared should always be positive, but it's possible for pred_y to be negative.
            if (any(is.na(pred_y))) ss_y = Inf
            ss = ss + ss_y
        }
        return (ss)
}

chi_difference_fun = function(obsered_df, x_name, predict_func) {
    function(...) {
        chi_difference(obsered_df, x_name, predict_func(...))
    }
}

# During integration, if the integrater does not provide a way to report progress then it is helpful to have the function being integrater report progress as it is called.
# Here a class is defined that will be called during each step of the integration and can be used to report progress.
# reporter
#   reporter(ode_func=NULL) - A constructor that accepts the function the reporter will monitor, or takes no arguments if it will be combined with other reporters.
#   update(state, t) - Update values in the reporter
#   reset() - Reset the reporter to its original state.
#   ode(t, state, parms) - A function to access ode_func().
#
# The ode() function must be present exactly like this so that it reporters can be combined.
#    ode = if(!is.null(ode_func)) {
#            function(t, state, parms) {
#                 result = ode_func(t, state, parms)
#                 update(c(state, parms, result[[2]]), t)
#                 result
#            }
#          } else {
#              NULL
#          }
#
# Other functions can be provided to access data stored during the integration.


# time_reporter
# Shows a progress bar that reporters the number of times the function has been called and the current time step.
#
# counter() - Returns the number times the reporter was updated (an integer).
# iter_times() - Returns the time step at each call of the reporter (a numeric vector, where the index of the vector is the interation step).
# update() - see reporter definition.
# reset() - see reporter definition.

time_reporter = function(ode_func=NULL) {
    stopifnot(is.null(ode_func) || is.function(ode_func))
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
        if (counter_ - last_update_ > 49) {
            setWinProgressBar(pb, value=0, label=sprintf("count: %d, time: %0.2f\n", counter_, t))
            last_update_ <<- counter_
        }
        invisible()
    }

    counter = function() { counter_ }

    iter_times = function () { iter_times_[!is.na(iter_times_)] }

    ode = if(!is.null(ode_func)) {
            function(t, state, parms) {
                result = ode_func(t, state, parms)
                update(c(state, parms, result[[2]]), t)
                result
            }
          } else {
              NULL
          }
    return (list(ode=ode, counter=counter, iter_times=iter_times, update=update, reset=reset))
}

# state_reporter
# Records the state during the integration.
# Some variable time step integraters use appropriate time steps to ensure the function is linear for any given time step, but then do not report the state at those times: e.g., many solvers in the deSolve package.
# That is bizarre behavior.
# This class attempts to determine appropriate times to store the state.
# The reporter can include unused integration steps, so it can be used to determine what the integrater is doing, but the data
# from it shouldn't be relied on. If you're using a bizarre solver, and you need to data at certain times,
# you could use this to determine appropriate times, and solve the system again using those times as input.
#
# state() - Returns a data frame of the state at every time step that is potentially important to get linearity between times. WARNING: It may contain integration steps that were not acctually used in solving the system.
# update() - see reporter definition.
# reset() - see reporter definition.

state_reporter = function(ode_func=NULL) {
    stopifnot(is.null(ode_func) || is.function(ode_func))
    state_list = list()
    times = numeric()
    state_index = 0L
    highest_index = 0L
    state_ = NULL
    updated = FALSE
    rows = 1e5

    reset = function() {
        state_list <<- list()
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
        if (length(state_list) == 0L) {  # Allocate memory by creating a list of empty elements.
            state_list <<- vector('list', rows)
            times <<- numeric(rows)
        }
        valid_times = times[seq_len(state_index)]

        # Set state_index to the location with a stored time just larger than the current time.
        # This effectively erases any stored values with times greater than the current time.
        state_index <<- as.integer(min(c(state_index + 1L, which(valid_times >= time)), na.rm=TRUE))

        if (state_index > highest_index) {
            highest_index <<- state_index  # Keep track of the higest recorded index, so values discarded by the integrater can be erased.
            if (state_index > rows) {  # If the amount of data is larger than space in the list, create a new, larger, list and copy everything to it.
                copy = state_list
                rows <<- rows * 2
                state_list <<- vector('list', rows)
                state_list[seq_len(state_index - 1)] <<- copy
            }
        }
        state_list[[state_index]] <<- unlist(state)
        times[state_index] <<- time
        invisible ()
    }

    state = function() {
        # It's slow to convert the list to a data frame, so keep a stored copy of the result and return that if update() hasn't been called.
        if (updated) {
            state_list[seq(state_index + 1L, highest_index + 1L)] <<- list(NULL)  # When storing values during update(), times that were discarded by the integrater are not removed, so remove them before returning the state.
            state_ <<- as.data.frame(do.call(rbind, state_list[!is.na(state_list)]))
            updated <<- FALSE
        }
        return (state_)
    }

    ode = if(!is.null(ode_func)) {
            function(t, state, parms) {
                result = ode_func(t, state, parms)
                update(c(state, parms, result[[2]]), t)
                result
            }
          } else {
              NULL
          }

    return (list(ode=ode, reset=reset, update=update, state=state))
}

# combine_reporters
# Accepts a list of reporters and returns a reporter that has all of the functions of the provided reporters.
# If more than 1 reporter provides functions with the same name, and each returns a value, a list of values with an element for each reporter. Otherwise, the return value is the same as if the single reporter had been used.
# E.g.,
# combo_reporter = combine_reporters(list(timer_reporter(), state_reporter()))
# combo_reporter$reset() - Resets both reporters
# combo_reporter$state() - Only the state_reporter() has this function, and it will return a data frame.
#
# You wouldn't do this, but for example if you use the following:
# combo_reporter = combine_reporters(list(state_reporter(), state_reporter()))
# combo_reproter$state() - Both state reporters has this function, and a list of two data frames is returned.

combine_reporters = function(reporter_list, ode_func) {
    stopifnot(is.list(reporter_list))
    stopifnot(is.function(reporter_list[[1]][['update']]))
    functions = list()

    stopifnot(is.function(ode_func))

    for (reporter in reporter_list) {
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

    # The ode() function is special in that it should only ever be called once for each reporter.
    # Run ode() once, and use its result to update all the reporters.
    func_calls$ode = function(t, state, parms) {
        result = ode_func(t, state, parms)
        func_calls$update(c(state, parms, result[[2]]), t)
        result
    }
    return (func_calls)
}

