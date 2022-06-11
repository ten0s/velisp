import os from 'os'
import {TestRunner} from './test-runner.js'
import {Str} from '../src/VeLispTypes.js'

TestRunner.run({
    name: 'vl-filename-mktemp',

    setup: () => {
        const oldtmp = process.env['TMP']
        const oldtemp = process.env['TEMP']
        delete process.env['TMP']
        delete process.env['TEMP']
        return {oldtmp, oldtemp}
    },

    teardown: ({oldtmp, oldtemp}) => {
        process.env['TMP'] = oldtmp
        process.env['TEMP'] = oldtemp
    },

    tests: [
        // Test `pattern`
        {test: '(setenv "TMP" (tmpdir)) (vl-filename-mktemp)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.tmpdir()}/velisp-`)
        }},
        {test: '(setenv "TMP" (tmpdir)) (vl-filename-mktemp "")', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.tmpdir()}/velisp-`)
        }},
        {test: '(setenv "TMP" (tmpdir)) (vl-filename-mktemp nil)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.tmpdir()}/velisp-`)
        }},
        {test: '(setenv "TMP" (tmpdir)) (vl-filename-mktemp T)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.tmpdir()}/velisp-`)
        }},
        {test: '(setenv "TMP" (tmpdir)) (vl-filename-mktemp nil nil nil)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.tmpdir()}/velisp-`)
        }},
        {test: '(setenv "TMP" (tmpdir)) (vl-filename-mktemp T T T)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.tmpdir()}/velisp-`)
        }},
        {test: '(setenv "TEMP" (tmpdir)) (vl-filename-mktemp)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.tmpdir()}/velisp-`)
        }},
        {test: '(setenv "TMP" "") (setenv "TEMP" "") (vl-filename-mktemp)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.tmpdir()}/velisp-`)
        }},
        {test: '(setenv "TMP" ".") (setenv "TEMP" ".") (vl-filename-mktemp)', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith('velisp-')
        }},

        // Test `directory`
        {test: '(setenv "TMP" (tmpdir)) (vl-filename-mktemp nil (homedir))', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().startsWith(`${os.homedir()}/velisp-`)
        }},

        // Test `extension`
        {test: '(vl-filename-mktemp nil nil ".text")', result: (act) => {
            //console.log(act)
            return act instanceof Str
                && act.value().endsWith('.text')
        }},
    ],

    errors: [
        {test: '(vl-filename-mktemp nil nil nil nil)', result:
         new Error('vl-filename-mktemp: too many arguments')},
        {test: '(vl-filename-mktemp \'pattern)', result:
         new Error('vl-filename-mktemp: `pattern` expected Str, Bool')},
        {test: '(vl-filename-mktemp "" \'directory)', result:
         new Error('vl-filename-mktemp: `directory` expected Str, Bool')},
    ]
})
