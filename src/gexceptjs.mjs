export function try_ffi(try_callback, catch_callback) {
    try {
       return try_callback();
    } catch(e) {
        return catch_callback(e);
    }
}

export function throw_ffi(exception) {
    throw exception;
}
