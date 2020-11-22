const {Bool, Int, Sym, Str, List, Fun, ensureType} = require('../VeLispTypes.js');
const Evaluator = require('../VeLispEvaluator.js');
const {VeDclContext} = require('../VeDclContext.js');
const VeDclLoader = require('../VeDclLoader.js');

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

const TileMode = {
    ENABLE_TILE: 0,
    DISABLE_TILE: 1,
    FOCUS_TILE: 2,
    SELECT_EDITBOX: 3,
    FLIP_IMAGE: 4
};

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
        const context = new VeDclContext();
        const jsDialogs = VeDclLoader.load(dclFile.value(), context);
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
  <requires lib="gtk+" version="3.20"/>
  ${gtkDialogXml}
</interface>
`;
                console.log(gtkXml);
                _gtkBuilder = new Gtk.Builder();
                _gtkBuilder.addFromString(gtkXml, gtkXml.length);
                try {
                    _gtkDialog = _gtkBuilder.getObject(dlgId.value());
                    for (let [key, handler] of jsDialog.getActions()) {
                        const tile = _gtkBuilder.getObject(key);
                        console.log(tile);
                        attachAction(tile, new Str(key), new Str(handler), context);
                    }
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
        _gtkDialog.setModal(true);
        _gtkDialog.setResizable(false);
        // TODO: calculate using both length and font
        console.log(_gtkDialog.getTitle().length);
        const fixMeWidth = Math.max(200, _gtkDialog.getTitle().length * 16);
        _gtkDialog.setSizeRequest(fixMeWidth, -1);
        _gtkDialog.on('show', Gtk.main);
        _gtkDialog.on('destroy', Gtk.mainQuit);
        _gtkDialog.showAll();
        // See done_dialog for dialogStatus
        const status = _gtkDialog.dialogStatus ? _gtkDialog.dialogStatus : 0;
        return new Int(status);
    }));
    context.setSym('DONE_DIALOG', new Fun('done_dialog', ['[status]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('done_dialog: too many arguments');
        }
        if (args.length == 1) {
            status = ensureType('done_dialog:', args[0], [Int]);
        }
        // TODO: check there's current dialog
        // See start_dialog for dialogStatus
        _gtkDialog.dialogStatus = status;
        Gtk.mainQuit();
        // TODO: what it should return? some (X, Y) point of the dialog
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
            //debugger;
            console.log(tile);
            attachAction(tile, key, handler, context);
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
            if (tile instanceof Gtk.Entry || tile instanceof Gtk.Label) {
                return new Str(tile.getText());
            } else if (tile instanceof Gtk.Button) {
                return new Str(tile.getLabel());
            } else {
                console.error(`Error: not implemented get_tile for ${typeof tile}`);
            }
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
            if (tile instanceof Gtk.Entry || tile instanceof Gtk.Label) {
                tile.setText(value.value());
            } else if (tile instanceof Gtk.Button) {
                return new Str(tile.setLabel(value.value()));
            } else {
                console.error(`Error: not implemented set_tile for ${typeof tile}`);
            }
            return value;
        } catch {
            // TODO: How to handle error?
            console.error(`Error: no tile found for '${key.value()}'`);
            return new Str('');
        }
    }));
    context.setSym('MODE_TILE', new Fun('mode_tile', ['key', 'mode'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('mode_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('mode_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('mode_tile: `key`', args[0], [Str]);
        const mode = ensureType('mode_tile: `mode`', args[1], [Int]);
        try {
            const tile = _gtkBuilder.getObject(key.value());
            switch (mode.value()) {
            case TileMode.ENABLE_TILE:
                tile.setSensitive(true);
                break;
            case TileMode.DISABLE_TILE:
                tile.setSensitive(false);
                break;
            case TileMode.FOCUS_TILE:
                _gtkDialog.setFocus(tile);
                break;
            case TileMode.FLIP_IMAGE:
                console.error(`Error: not implemented tile mode '${mode.value()}'`);
                break;
            default:
                console.error(`Error: unknown tile mode '${mode.value()}'`);
            }
        } catch {
            // TODO: How to handle error?
            console.error(`Error: no tile found for '${key.value()}'`);
        }
        return new Bool(false);
    }));
}

const attachAction = (tile, key, handler, context) => {
    let event = null;
    if (tile instanceof Gtk.Button) {
        event = 'clicked';
    } else if (tile instanceof Gtk.Entry) {
        event = 'changed';
    } else {
        throw new Error(`Error: not event found for '${tile}'`);
    }
    // TODO: support other events depending on tile type
    // TODO: how to unregister registered in DCL action = "(...)"?
    tile.on(event, () => {
        context.setVar('$KEY', key);
        context.setVar('$VALUE', new Str(tile.getText ? tile.getText() : ''));
        // TODO: add $REASON
        // https://help.autodesk.com/view/OARX/2019/ENU/?guid=GUID-0473B723-1CD5-4228-AB25-D88B6930372F
        // TODO: add $DATA (client_data_tile ...)
        // TODO: add $X, $Y for image_button
        Evaluator.evaluate(handler.toUnescapedString(), context);
    });
}
