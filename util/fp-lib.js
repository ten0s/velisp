// Poor man's FP lib

const compose = (...fns) => (x) => fns.reduceRight((acc, fn) => fn(acc), x)
const filter = (fn) => (arr) => arr.filter(fn)
const forEach = (fn) => (arr) => arr.forEach(fn)
const map = (fn) => (arr) => arr.map(fn)
const pipe = (...fns) => x => fns.reduce((acc, fn) => fn(acc), x)
const prop = (name) => (obj) => obj[name]
const tap = (fn) => (x) => { fn(x); return x }

const startsWith = (s) => (x) => x.startsWith(s)
const endsWith = (s) => (x) => x.endsWith(s)
const trim = (x) => x.trim()

export {
    compose,
    filter,
    forEach,
    map,
    pipe,
    prop,
    tap,

    startsWith,
    endsWith,
    trim,
}
