const not = func => (x) => !(func(x))

const contains = arr => item =>
    arr.indexOf(item) !== -1

if (!Array.prototype.with) {
    Array.prototype.with = function without(items) {
        return this.filter(contains(items))
    }
}

if (!Array.prototype.without) {
    Array.prototype.without = function without(items) {
        return this.filter(not(contains(items)))
    }
}
