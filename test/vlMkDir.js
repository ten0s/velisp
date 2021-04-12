const fs = require('fs')
const {TestRunner} = require('./test-runner.js')
const {Bool} = require('../src/VeLispTypes.js')

TestRunner.run({
    name: 'vl-mkdir',

    setup: () => {
        fs.mkdirSync('t2')
    },

    teardown: () => {
        fs.rmdirSync('t1/a/b')
        fs.rmdirSync('t1/a')
        fs.rmdirSync('t1')
        fs.rmdirSync('t2')
    },

    tests: [
        {test: '(vl-mkdir "t1")', result: new Bool(true)},
        {test: '(vl-mkdir "t2")', result: new Bool(false)},
        {test: '(vl-mkdir "t1/a/b")', result: new Bool(false)},
        {test: '(vl-mkdir "t1") (vl-mkdir "t1/a") (vl-mkdir "t1/a/b")', result: new Bool(true)},
    ],

    errors: [
        {test: '(vl-mkdir)', result: new Error('vl-mkdir: too few arguments')},
        {test: '(vl-mkdir "t1" "t2")', result: new Error('vl-mkdir: too many arguments')},
        {test: '(vl-mkdir \'t1)', result: new Error('vl-mkdir: expected Str')},
    ]
})
