// Poor man's FP lib

const filter = (fn) => (arr) => arr.filter(fn)
const forEach = (fn) => (arr) => arr.forEach(fn)
const map = (fn) => (arr) => arr.map(fn)
const pipe = (...fns) => x => fns.reduce((acc, fn) => fn(acc), x)
const prop = (name) => (obj) => obj[name]
const tap = (fn) => (x) => { fn(x); return x }

export {
    filter,
    forEach,
    map,
    pipe,
    prop,
    tap,
}
