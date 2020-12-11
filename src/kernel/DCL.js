const {Bool, Int, Sym, Str, List, Fun, ensureType} = require('../VeLispTypes.js');
const {VeDclContext} = require('../VeDclContext.js');
const VeDclLoader = require('../VeDclLoader.js');
const {ListOperation} = require('../VeDclTiles.js');

const util = require('util');

// global dclId index
let _dclId = 0;
const _dclFiles = {};

let _dclDialog = null;
let _listHandle = null;
let _imageHandle = null;

exports.initContext = function (context) {
    context.setSym('ALERT', new Fun('alert', ['string'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('alert: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('alert: too many arguments');
        }
        const str = ensureType('alert:', args[0], [Str]);
        // TODO: Re-implement alert in acad.dcl
        // TODO: Doesn't work reliably w/o parent
        const gi = require('node-gtk');
        const Gtk = gi.require('Gtk', '3.0');
        const dlg = new Gtk.MessageDialog({
            title: "Alert",
            text: str.value(),
            message_type: Gtk.MessageType.INFO,
            buttons: Gtk.ButtonsType.OK
        });
        if (_dclDialog) {
            // FIXME: get rid of internal knowledge
            dlg.transientFor = _dclDialog._gtkWindow;
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

        // Inject lib/dcl/{base,acad}.dcl
        const path = require('path');
        let rootdir = path.join(__dirname, '../..');
        // Win32 workaround
        rootdir = rootdir.split('\\').join('/');
        VeDclLoader.load(`${rootdir}/lib/dcl/base.dcl`, context);
        VeDclLoader.load(`${rootdir}/lib/dcl/acad.dcl`, context);

        const dclDialogs = VeDclLoader.load(dclFile.value(), context);
        //console.log(util.inspect(dclDialogs, {showHidden: false, depth: null}));
        const dclMap = {};
        for (const dclDialog of dclDialogs) {
            dclMap[dclDialog.id] = dclDialog;
        }
        const dclId = new Int(_dclId++);
        _dclFiles[dclId.value()] = dclMap;
        return dclId;
    }));
    context.setSym('NEW_DIALOG', new Fun('new_dialog', ['dlg_id', 'dcl_id', '[action]', '[point]'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('new_dialog: too few arguments');
        }
        if (args.length > 4) {
            throw new Error('new_dialog: too many arguments');
        }
        const dlgId = ensureType('new_dialog: `dlg_id`', args[0], [Str]);
        const dclId = ensureType('new_dialog: `dcl_id`', args[1], [Int]);
        let action = new Str('');
        let point = new List([new Int(-1), new Int(-1)]);
        if (args.length > 2) {
            action = ensureType('new_dialog: `action`', args[2], [Str]);
        }
        if (args.length > 3) {
            point = ensureType('new_dialog: `point`', args[3], [List]);
        }
        const dclFile = _dclFiles[dclId.value()];
        if (dclFile) {
            const dclDialog = dclFile[dlgId.value()];
            if (dclDialog) {
                try {
                    const position = [point.value()[0].value(), point.value()[1].value()];
                    _dclDialog = dclDialog.clone();
                    _dclDialog.gtkInitWidget(action.value(), position, context);
                    return new Bool(true);
                } catch (e) {
                    // Should never happen since dialog ID is mandatory
                    console.error(e);
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
        // TODO: ensure _dclDialog
        const status = _dclDialog.startDialog();
        return new Int(status);
    }));
    context.setSym('DONE_DIALOG', new Fun('done_dialog', ['[status]'], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('done_dialog: too many arguments');
        }
        let status = new Int(0);
        if (args.length == 1) {
            status = ensureType('done_dialog:', args[0], [Int]);
        }
        // TODO: check there's current dialog
        const [x, y] = _dclDialog.doneDialog(status.value());
        return new List([new Int(x), new Int(y)]);
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
    context.setSym('ACTION_TILE', new Fun('action_tile', ['key', 'action'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('action_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('action_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('action_tile: `key`', args[0], [Str]);
        const action = ensureType('action_tile: `action`', args[1], [Str]);
        //console.log(action.toUnescapedString());
        try {
            _dclDialog.actionTile(key.value(), action.value(), context);
            return new Bool(true);
        } catch (e) {
            console.error(e);
            return new Bool(false);
        }
    }));
    context.setSym('CLIENT_DATA_TILE', new Fun('client_data_tile', ['key', 'data'], [], (self, args) => {
        if (args.length < 2) {
            throw new Error('client_data_tile: too few arguments');
        }
        if (args.length > 2) {
            throw new Error('client_data_tile: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('client_data_tile: `key`', args[0], [Str]);
        const data = ensureType('client_data_tile: `data`', args[1], [Str]);
        _dclDialog.clientDataTile(key.value(), data.value());
        return new Bool(false);
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
        const str = _dclDialog.getTile(key.value());
        return new Str(str);
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
        _dclDialog.setTile(key.value(), value.value());
        return value;
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
        _dclDialog.modeTile(key.value(), mode.value());
        return new Bool(false);
    }));
    context.setSym('START_LIST', new Fun('start_list', ['key', '[operation]', '[index]'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('start_list: too few arguments');
        }
        if (args.length > 3) {
            throw new Error('start_list: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('start_list: `key`', args[0], [Str]);
        let operation;
        if (args.length > 1) {
            operation = ensureType('start_list: `operation`', args[1], [Int]);
        } else {
            operation = new Int(ListOperation.CLEAR);
        }
        let index;
        if (args.length > 2) {
            index = ensureType('start_list: `index`', args[2], [Int]);
        } else {
            // Used for ListOperation.CHANGE only
            index = new Int(0);
        }
        _listHandle = _dclDialog.startList(key.value(), operation.value(), index.value());
        // TODO: The key argument, if successful; otherwise nil.
        return key;
    }));
    context.setSym('ADD_LIST', new Fun('add_list', ['str'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('add_list: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('add_list: too many arguments');
        }
        // TODO: ensure current dialog
        // TODO: ensure list handle
        const str = ensureType('add_list: `str`', args[0], [Str]);
        _dclDialog.addList(_listHandle, str.value());
        // TODO: return nil on error
        return str;
    }));
    context.setSym('END_LIST', new Fun('end_list', [], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('end_list: too many arguments');
        }
        // TODO: ensure current dialog
        // TODO: ensure list handle
        _dclDialog.endList(_listHandle);
        _listHandle = null;
        return new Bool(false);
    }));
    context.setSym('DIMX', new Fun('dimx', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('dimx: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('dimx: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('dimx: `key`', args[0], [Str]);
        const dimX = _dclDialog.dimX(key.value());
        return new Int(dimX);
    }));
    context.setSym('DIMY', new Fun('dimy', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('dimy: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('dimy: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('dimy: `key`', args[0], [Str]);
        const dimY = _dclDialog.dimY(key.value());
        return new Int(dimY);
    }));
    context.setSym('START_IMAGE', new Fun('start_list', ['key'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('start_image: too few arguments');
        }
        if (args.length > 3) {
            throw new Error('start_image: too many arguments');
        }
        // TODO: ensure current dialog
        const key = ensureType('start_image: `key`', args[0], [Str]);
        _imageHandle = _dclDialog.startImage(key.value());
        // TODO: The key argument, if successful; otherwise nil.
        return key;
    }));
    context.setSym('FILL_IMAGE', new Fun('fill_image', ['x1', 'y1', 'width', 'height', 'color'], [], (self, args) => {
        if (args.length < 5) {
            throw new Error('fill_image: too few arguments');
        }
        if (args.length > 5) {
            throw new Error('fill_image: too many arguments');
        }
        // TODO: ensure current dialog
        // TODO: ensure image handle
        const x = ensureType('fill_image: `x1`'    , args[0], [Int]);
        const y = ensureType('fill_image: `y1`'    , args[1], [Int]);
        const w = ensureType('fill_image: `width`' , args[2], [Int]);
        const h = ensureType('fill_image: `height`', args[3], [Int]);
        const c = ensureType('fill_image: `color`' , args[4], [Int]);
        _dclDialog.fillImage(
            _imageHandle, x.value(), y.value(), w.value(), h.value(), c.value()
        );
        return c;
    }));
    context.setSym('VECTOR_IMAGE', new Fun('vector_image', ['x1', 'y1', 'x2', 'y2', 'color'], [], (self, args) => {
        if (args.length < 5) {
            throw new Error('vector_image: too few arguments');
        }
        if (args.length > 5) {
            throw new Error('vector_image: too many arguments');
        }
        // TODO: ensure current dialog
        // TODO: ensure image handle
        const x1 = ensureType('vector_image: `x1`'   , args[0], [Int]);
        const y1 = ensureType('vector_image: `y1`'   , args[1], [Int]);
        const x2 = ensureType('vector_image: `x2`'   , args[2], [Int]);
        const y2 = ensureType('vector_image: `y2`'   , args[3], [Int]);
        const c  = ensureType('vector_image: `color`', args[4], [Int]);
        _dclDialog.vectorImage(
            _imageHandle, x1.value(), y1.value(), x2.value(), y2.value(), c.value()
        );
        return c;
    }));
    context.setSym('END_IMAGE', new Fun('end_image', [], [], (self, args) => {
        if (args.length > 1) {
            throw new Error('end_image: too many arguments');
        }
        // TODO: ensure current dialog
        // TODO: ensure image handle
        _dclDialog.endImage(_imageHandle);
        _imageHandle = null;
        return new Bool(false);
    }));
}
