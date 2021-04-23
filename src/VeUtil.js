// :: (any, [any]) -> bool
const find = (y, xs) => {
    for (let x of xs) {
        if (y === x) {
            return true
        }
    }
    return false
}

module.exports = {
    find,
}
