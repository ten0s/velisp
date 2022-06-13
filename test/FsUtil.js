import fs from 'fs'

const touch = (f) => fs.closeSync(fs.openSync(f, 'w'))
const rm    = (f) => fs.rmSync(f, {force: true})
const rmdir = (d) => fs.rmdirSync(d)

const doTry = (func) => (arg) => { try { func(arg) } catch {} }

const tryTouch = doTry(touch)
const tryRm    = doTry(rm)
const tryRmdir = doTry(rmdir)

export {
    touch,
    rm,
    rmdir,

    tryTouch,
    tryRm,
    tryRmdir,
}
