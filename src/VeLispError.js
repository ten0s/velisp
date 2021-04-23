function fmtError(name, error) {
    //console.error(error);
    let message = `${name}: `
    if (error.path)  {
        message += `${error.path}: `
    }
    if (error.code)  {
        message += `${perror(error.code)}`
    } else {
        message += `${error.message}`
    }
    return message
}

function perror(errCode) {
    switch(errCode) {
    case 'EACCES':
        return 'Permission denied'
    case 'EEXIST':
        return 'File exists'
    case 'EISDIR':
        return 'Is a directory'
    case 'EMFILE':
        return 'Too many open files in system'
    case 'ENOENT':
        return 'No such file or directory'
    case 'ENOTDIR':
        return 'Not a directory'
    case 'ETIMEDOUT':
        return 'Operation timed out'
    default:
        return errCode
    }
}

module.exports = {
    fmtError,
}
