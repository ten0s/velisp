// -*- mode: js -*-

// To workaround
// https://github.com/vercel/pkg/issues/1404

// import {fileURLToPath} from 'url'
// const __filename = fileURLToPath(import.meta.url)
// const __dirname = path.dirname(__filename)

// Keep the module in the /src directory!

const path = require('path')
const __rootdir = path.dirname(__dirname)

module.exports = __rootdir
