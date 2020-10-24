const {Bool, Int, Sym, Str, List, Fun, ensureType} = require('../VeLispTypes.js');
const Evaluator = require('../VeLispEvaluator.js');
const VeDclDialogsLoader = require('../VeDclDialogsLoader.js');

const util = require('util');

const gi = require('node-gtk');
const Gtk = gi.require('Gtk', '3.0');

gi.startLoop();
Gtk.init();

// global dclId index
let _dclId = 0;
const _dclFiles = {};

let _gtkBuilder = null;
let _gtkDialog = null;

exports.initContext = function (context) {
    context.setSym('ALERT', new Fun('alert', ['string'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('alert: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('alert: too many arguments');
        }
        const str = ensureType('alert:', args[0], [Str]);
        // TODO: Doesn't work reliably w/o parent
        const dlg = new Gtk.MessageDialog({
            title: "Alert",
            text: str.value(),
            message_type: Gtk.MessageType.INFO,
            buttons: Gtk.ButtonsType.OK
        });
        if (_gtkDialog) {
            dlg.transientFor = _gtkDialog;
        }
        dlg.run();
        dlg.destroy();
        return new Bool(false);
    }));
    context.setSym('LOAD_DIALOG', new Fun('load_dialog', ['dclfile'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('load_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('load_dialog: too many arguments');
        }
        const dclFile = ensureType('load_dialog:', args[0], [Str]);
        // TODO: add .dcl extension if not provided
        // TODO: ensure file exists and loads,
        // return a positive integer, or a negative integer on error
        const jsDialogs = VeDclDialogsLoader.load(dclFile.value());
        console.log(util.inspect(jsDialogs, {showHidden: false, depth: null}));
        const dclMap = {};
        for (const jsDialog of jsDialogs) {
            dclMap[jsDialog.id] = jsDialog;
        }
        const dclId = new Int(_dclId++);
        _dclFiles[dclId.value()] = dclMap;
        return dclId;
    }));
    context.setSym('NEW_DIALOG', new Fun('new_dialog', ['dlg_id', 'dcl_id'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('new_dialog: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('new_dialog: too many arguments');
        }
        const dclId = ensureType('new_dialog: `dcl_id`', args[1], [Int]);
        const dclFile = _dclFiles[dclId.value()];
        if (dclFile) {
            const dlgId = ensureType('new_dialog: `dlg_id`', args[0], [Str]);
            const jsDialog = dclFile[dlgId.value()];
            if (jsDialog) {
                const gtkDialogXml = jsDialog.toGtkXml();
                const gtkXml = `
<?xml version="1.0" encoding="UTF-8"?>
<interface>
${gtkDialogXml}
</interface>
`;
                debugger;
                console.log(gtkXml);
                _gtkBuilder = new Gtk.Builder();
                _gtkBuilder.addFromString(gtkXml, gtkXml.length);
                try {
                    _gtkDialog = _gtkBuilder.getObject(dlgId.value());
                    return new Bool(true);
                } catch {
                    // Should never happen since dialog ID is mandatory
                }
            } else {
                // TODO: No dlg_id found in DCL file
            }
        } else {
            // TODO: No dcl_id found
        }
        return new Bool(false);
    }));
    context.setSym('START_DIALOG', new Fun('start_dialog', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('start_dialog: too many arguments');
        }
        // TODO: ensure _gtkDialog
        const ret = _gtkDialog.run();
        console.log(ret);
        return new Int(ret);
    }));
    context.setSym('DONE_DIALOG', new Fun('done_dialog', ['[status]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('done_dialog: too many arguments');
        }
        if (args.length == 1) {
            const status = ensureType('done_dialog:', args[0], [Int]);
            // TODO: where to put it?
        }
        // TODO: check there's current dialog
        // TODO: what it should return? some (X, Y) point of the dialog
        _gtkDialog.destroy();
        return new Bool(true);
    }));
    context.setSym('UNLOAD_DIALOG', new Fun('unload_dialog', ['dcl_id'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('unload_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('unload_dialog: too many arguments');
        }
        const dclId = ensureType('unload_dialog:', args[0], [Int]);
        delete _dclFiles[dclId.value()];
        return new Bool(false);
    }));
    context.setSym('ACTION_TILE', new Fun('action_tile', ['key', 'handler'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('action_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('action_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('action_tile: `key`', args[0], [Str]);
        const handler = ensureType('action_tile: `handler`', args[1], [Str]);
        console.log(handler.toUnescapedString());
        try {
            const tile = _gtkBuilder.getObject(key.value());
            // TODO: support other events depending on tile type
            tile.on('clicked', () => {
                Evaluator.evaluate(handler.toUnescapedString(), context);
            });
            return new Bool(true);
        } catch {
            return new Bool(false);
        }
    }));
    context.setSym('GET_TILE', new Fun('get_tile', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('get_tile: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('get_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('get_tile:', args[0], [Str]);
        try {
            const tile = _gtkBuilder.getObject(key.value());
            return new Str(tile.getText());
        } catch {
            // TODO: How to handle error?
            console.error(`Error: no tile found for '${key.value()}'`);
            return new Bool(false);
        }
    }));
    context.setSym('SET_TILE', new Fun('get_tile', ['key', 'value'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('set_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('set_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('set_tile:', args[0], [Str]);
        const value = ensureType('set_tile:', args[1], [Str]);
        try {
            const tile = _gtkBuilder.getObject(key.value());
            tile.setText(value.value());
            return value;
        } catch {
            // TODO: How to handle error?
            console.error(`Error: no tile found for '${key.value()}'`);
            return new Str('');
        }
    }));
}
