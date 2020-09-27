// Generated from grammar/VeLisp.g4 by ANTLR 4.8
// jshint ignore: start
var antlr4 = require('antlr4/index');
var VeLispVisitor = require('./VeLispVisitor').VeLispVisitor;

var grammarFileName = "VeLisp.g4";


var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003\u001d\u00ff\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004",
    "\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007",
    "\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004\f\t\f",
    "\u0004\r\t\r\u0004\u000e\t\u000e\u0004\u000f\t\u000f\u0004\u0010\t\u0010",
    "\u0004\u0011\t\u0011\u0004\u0012\t\u0012\u0003\u0002\u0007\u0002&\n",
    "\u0002\f\u0002\u000e\u0002)\u000b\u0002\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0007\u0003.\n\u0003\f\u0003\u000e\u00031\u000b\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u00037\n\u0003\f\u0003\u000e",
    "\u0003:\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0007\u0003B\n\u0003\f\u0003\u000e\u0003E\u000b",
    "\u0003\u0003\u0003\u0003\u0003\u0007\u0003I\n\u0003\f\u0003\u000e\u0003",
    "L\u000b\u0003\u0005\u0003N\n\u0003\u0003\u0003\u0003\u0003\u0006\u0003",
    "R\n\u0003\r\u0003\u000e\u0003S\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003]\n\u0003\f\u0003",
    "\u000e\u0003`\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0005\u0003i\n\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007",
    "\u0003q\n\u0003\f\u0003\u000e\u0003t\u000b\u0003\u0003\u0003\u0003\u0003",
    "\u0007\u0003x\n\u0003\f\u0003\u000e\u0003{\u000b\u0003\u0005\u0003}",
    "\n\u0003\u0003\u0003\u0003\u0003\u0006\u0003\u0081\n\u0003\r\u0003\u000e",
    "\u0003\u0082\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0007\u0003\u008a\n\u0003\f\u0003\u000e\u0003\u008d\u000b\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003\u0093\n\u0003",
    "\f\u0003\u000e\u0003\u0096\u000b\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0007\u0003\u00a2\n\u0003\f\u0003\u000e\u0003\u00a5",
    "\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0007\u0003\u00ac\n\u0003\f\u0003\u000e\u0003\u00af\u000b\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0006\u0003\u00b6",
    "\n\u0003\r\u0003\u000e\u0003\u00b7\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0006\u0003\u00be\n\u0003\r\u0003\u000e\u0003\u00bf\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007",
    "\u0003\u00c8\n\u0003\f\u0003\u000e\u0003\u00cb\u000b\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0005\u0003\u00d6\n\u0003\u0003\u0004\u0003",
    "\u0004\u0003\u0004\u0007\u0004\u00db\n\u0004\f\u0004\u000e\u0004\u00de",
    "\u000b\u0004\u0003\u0004\u0003\u0004\u0003\u0005\u0003\u0005\u0003\u0006",
    "\u0003\u0006\u0003\u0007\u0003\u0007\u0003\b\u0003\b\u0003\t\u0003\t",
    "\u0003\n\u0003\n\u0003\u000b\u0003\u000b\u0003\f\u0003\f\u0003\r\u0003",
    "\r\u0003\u000e\u0003\u000e\u0003\u000f\u0003\u000f\u0003\u0010\u0003",
    "\u0010\u0003\u0010\u0003\u0011\u0003\u0011\u0003\u0012\u0003\u0012\u0003",
    "\u0012\u0002\u0002\u0013\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014",
    "\u0016\u0018\u001a\u001c\u001e \"\u0002\u0002\u0002\u0116\u0002\'\u0003",
    "\u0002\u0002\u0002\u0004\u00d5\u0003\u0002\u0002\u0002\u0006\u00d7\u0003",
    "\u0002\u0002\u0002\b\u00e1\u0003\u0002\u0002\u0002\n\u00e3\u0003\u0002",
    "\u0002\u0002\f\u00e5\u0003\u0002\u0002\u0002\u000e\u00e7\u0003\u0002",
    "\u0002\u0002\u0010\u00e9\u0003\u0002\u0002\u0002\u0012\u00eb\u0003\u0002",
    "\u0002\u0002\u0014\u00ed\u0003\u0002\u0002\u0002\u0016\u00ef\u0003\u0002",
    "\u0002\u0002\u0018\u00f1\u0003\u0002\u0002\u0002\u001a\u00f3\u0003\u0002",
    "\u0002\u0002\u001c\u00f5\u0003\u0002\u0002\u0002\u001e\u00f7\u0003\u0002",
    "\u0002\u0002 \u00fa\u0003\u0002\u0002\u0002\"\u00fc\u0003\u0002\u0002",
    "\u0002$&\u0005\u0004\u0003\u0002%$\u0003\u0002\u0002\u0002&)\u0003\u0002",
    "\u0002\u0002\'%\u0003\u0002\u0002\u0002\'(\u0003\u0002\u0002\u0002(",
    "\u0003\u0003\u0002\u0002\u0002)\'\u0003\u0002\u0002\u0002*+\u0007\u0003",
    "\u0002\u0002+/\u0007\b\u0002\u0002,.\u0005\u0004\u0003\u0002-,\u0003",
    "\u0002\u0002\u0002.1\u0003\u0002\u0002\u0002/-\u0003\u0002\u0002\u0002",
    "/0\u0003\u0002\u0002\u000202\u0003\u0002\u0002\u00021/\u0003\u0002\u0002",
    "\u00022\u00d6\u0007\u0004\u0002\u000234\u0007\u0003\u0002\u000248\u0007",
    "\t\u0002\u000257\u0005\u0006\u0004\u000265\u0003\u0002\u0002\u00027",
    ":\u0003\u0002\u0002\u000286\u0003\u0002\u0002\u000289\u0003\u0002\u0002",
    "\u00029;\u0003\u0002\u0002\u0002:8\u0003\u0002\u0002\u0002;\u00d6\u0007",
    "\u0004\u0002\u0002<=\u0007\u0003\u0002\u0002=>\u0007\n\u0002\u0002>",
    "?\u0005\f\u0007\u0002?C\u0007\u0003\u0002\u0002@B\u0005\u000e\b\u0002",
    "A@\u0003\u0002\u0002\u0002BE\u0003\u0002\u0002\u0002CA\u0003\u0002\u0002",
    "\u0002CD\u0003\u0002\u0002\u0002DM\u0003\u0002\u0002\u0002EC\u0003\u0002",
    "\u0002\u0002FJ\u0007\u0005\u0002\u0002GI\u0005\u0010\t\u0002HG\u0003",
    "\u0002\u0002\u0002IL\u0003\u0002\u0002\u0002JH\u0003\u0002\u0002\u0002",
    "JK\u0003\u0002\u0002\u0002KN\u0003\u0002\u0002\u0002LJ\u0003\u0002\u0002",
    "\u0002MF\u0003\u0002\u0002\u0002MN\u0003\u0002\u0002\u0002NO\u0003\u0002",
    "\u0002\u0002OQ\u0007\u0004\u0002\u0002PR\u0005\u0004\u0003\u0002QP\u0003",
    "\u0002\u0002\u0002RS\u0003\u0002\u0002\u0002SQ\u0003\u0002\u0002\u0002",
    "ST\u0003\u0002\u0002\u0002TU\u0003\u0002\u0002\u0002UV\u0007\u0004\u0002",
    "\u0002V\u00d6\u0003\u0002\u0002\u0002WX\u0007\u0003\u0002\u0002XY\u0007",
    "\u000b\u0002\u0002YZ\u0005\u0012\n\u0002Z^\u0005\u0014\u000b\u0002[",
    "]\u0005\u0004\u0003\u0002\\[\u0003\u0002\u0002\u0002]`\u0003\u0002\u0002",
    "\u0002^\\\u0003\u0002\u0002\u0002^_\u0003\u0002\u0002\u0002_a\u0003",
    "\u0002\u0002\u0002`^\u0003\u0002\u0002\u0002ab\u0007\u0004\u0002\u0002",
    "b\u00d6\u0003\u0002\u0002\u0002cd\u0007\u0003\u0002\u0002de\u0007\f",
    "\u0002\u0002ef\u0005\u0016\f\u0002fh\u0005\u0018\r\u0002gi\u0005\u001a",
    "\u000e\u0002hg\u0003\u0002\u0002\u0002hi\u0003\u0002\u0002\u0002ij\u0003",
    "\u0002\u0002\u0002jk\u0007\u0004\u0002\u0002k\u00d6\u0003\u0002\u0002",
    "\u0002lm\u0007\u0003\u0002\u0002mn\u0007\r\u0002\u0002nr\u0007\u0003",
    "\u0002\u0002oq\u0005\u000e\b\u0002po\u0003\u0002\u0002\u0002qt\u0003",
    "\u0002\u0002\u0002rp\u0003\u0002\u0002\u0002rs\u0003\u0002\u0002\u0002",
    "s|\u0003\u0002\u0002\u0002tr\u0003\u0002\u0002\u0002uy\u0007\u0005\u0002",
    "\u0002vx\u0005\u0010\t\u0002wv\u0003\u0002\u0002\u0002x{\u0003\u0002",
    "\u0002\u0002yw\u0003\u0002\u0002\u0002yz\u0003\u0002\u0002\u0002z}\u0003",
    "\u0002\u0002\u0002{y\u0003\u0002\u0002\u0002|u\u0003\u0002\u0002\u0002",
    "|}\u0003\u0002\u0002\u0002}~\u0003\u0002\u0002\u0002~\u0080\u0007\u0004",
    "\u0002\u0002\u007f\u0081\u0005\u0004\u0003\u0002\u0080\u007f\u0003\u0002",
    "\u0002\u0002\u0081\u0082\u0003\u0002\u0002\u0002\u0082\u0080\u0003\u0002",
    "\u0002\u0002\u0082\u0083\u0003\u0002\u0002\u0002\u0083\u0084\u0003\u0002",
    "\u0002\u0002\u0084\u0085\u0007\u0004\u0002\u0002\u0085\u00d6\u0003\u0002",
    "\u0002\u0002\u0086\u0087\u0007\u0003\u0002\u0002\u0087\u008b\u0007\u000e",
    "\u0002\u0002\u0088\u008a\u0005\u0004\u0003\u0002\u0089\u0088\u0003\u0002",
    "\u0002\u0002\u008a\u008d\u0003\u0002\u0002\u0002\u008b\u0089\u0003\u0002",
    "\u0002\u0002\u008b\u008c\u0003\u0002\u0002\u0002\u008c\u008e\u0003\u0002",
    "\u0002\u0002\u008d\u008b\u0003\u0002\u0002\u0002\u008e\u00d6\u0007\u0004",
    "\u0002\u0002\u008f\u0090\u0007\u0003\u0002\u0002\u0090\u0094\u0007\u000f",
    "\u0002\u0002\u0091\u0093\u0005\u0004\u0003\u0002\u0092\u0091\u0003\u0002",
    "\u0002\u0002\u0093\u0096\u0003\u0002\u0002\u0002\u0094\u0092\u0003\u0002",
    "\u0002\u0002\u0094\u0095\u0003\u0002\u0002\u0002\u0095\u0097\u0003\u0002",
    "\u0002\u0002\u0096\u0094\u0003\u0002\u0002\u0002\u0097\u00d6\u0007\u0004",
    "\u0002\u0002\u0098\u0099\u0007\u0003\u0002\u0002\u0099\u009a\u0007\u0010",
    "\u0002\u0002\u009a\u009b\u0005\u0004\u0003\u0002\u009b\u009c\u0007\u0004",
    "\u0002\u0002\u009c\u00d6\u0003\u0002\u0002\u0002\u009d\u009e\u0007\u0003",
    "\u0002\u0002\u009e\u009f\u0007\u0011\u0002\u0002\u009f\u00a3\u0005\u001c",
    "\u000f\u0002\u00a0\u00a2\u0005\u0004\u0003\u0002\u00a1\u00a0\u0003\u0002",
    "\u0002\u0002\u00a2\u00a5\u0003\u0002\u0002\u0002\u00a3\u00a1\u0003\u0002",
    "\u0002\u0002\u00a3\u00a4\u0003\u0002\u0002\u0002\u00a4\u00a6\u0003\u0002",
    "\u0002\u0002\u00a5\u00a3\u0003\u0002\u0002\u0002\u00a6\u00a7\u0007\u0004",
    "\u0002\u0002\u00a7\u00d6\u0003\u0002\u0002\u0002\u00a8\u00a9\u0007\u0003",
    "\u0002\u0002\u00a9\u00ad\u0007\u0012\u0002\u0002\u00aa\u00ac\u0005\u001e",
    "\u0010\u0002\u00ab\u00aa\u0003\u0002\u0002\u0002\u00ac\u00af\u0003\u0002",
    "\u0002\u0002\u00ad\u00ab\u0003\u0002\u0002\u0002\u00ad\u00ae\u0003\u0002",
    "\u0002\u0002\u00ae\u00b0\u0003\u0002\u0002\u0002\u00af\u00ad\u0003\u0002",
    "\u0002\u0002\u00b0\u00d6\u0007\u0004\u0002\u0002\u00b1\u00b2\u0007\u0003",
    "\u0002\u0002\u00b2\u00b3\u0007\u0013\u0002\u0002\u00b3\u00b5\u0005 ",
    "\u0011\u0002\u00b4\u00b6\u0005\u0004\u0003\u0002\u00b5\u00b4\u0003\u0002",
    "\u0002\u0002\u00b6\u00b7\u0003\u0002\u0002\u0002\u00b7\u00b5\u0003\u0002",
    "\u0002\u0002\u00b7\u00b8\u0003\u0002\u0002\u0002\u00b8\u00b9\u0003\u0002",
    "\u0002\u0002\u00b9\u00ba\u0007\u0004\u0002\u0002\u00ba\u00d6\u0003\u0002",
    "\u0002\u0002\u00bb\u00bd\u0007\u0003\u0002\u0002\u00bc\u00be\u0005\"",
    "\u0012\u0002\u00bd\u00bc\u0003\u0002\u0002\u0002\u00be\u00bf\u0003\u0002",
    "\u0002\u0002\u00bf\u00bd\u0003\u0002\u0002\u0002\u00bf\u00c0\u0003\u0002",
    "\u0002\u0002\u00c0\u00c1\u0003\u0002\u0002\u0002\u00c1\u00c2\u0007\u0006",
    "\u0002\u0002\u00c2\u00c3\u0005\"\u0012\u0002\u00c3\u00c4\u0007\u0004",
    "\u0002\u0002\u00c4\u00d6\u0003\u0002\u0002\u0002\u00c5\u00c9\u0007\u0003",
    "\u0002\u0002\u00c6\u00c8\u0005\"\u0012\u0002\u00c7\u00c6\u0003\u0002",
    "\u0002\u0002\u00c8\u00cb\u0003\u0002\u0002\u0002\u00c9\u00c7\u0003\u0002",
    "\u0002\u0002\u00c9\u00ca\u0003\u0002\u0002\u0002\u00ca\u00cc\u0003\u0002",
    "\u0002\u0002\u00cb\u00c9\u0003\u0002\u0002\u0002\u00cc\u00d6\u0007\u0004",
    "\u0002\u0002\u00cd\u00d6\u0007\u0014\u0002\u0002\u00ce\u00d6\u0007\u0015",
    "\u0002\u0002\u00cf\u00d6\u0007\u0016\u0002\u0002\u00d0\u00d6\u0007\u0017",
    "\u0002\u0002\u00d1\u00d6\u0007\u0018\u0002\u0002\u00d2\u00d6\u0007\u0019",
    "\u0002\u0002\u00d3\u00d4\u0007\u0007\u0002\u0002\u00d4\u00d6\u0005\u0004",
    "\u0003\u0002\u00d5*\u0003\u0002\u0002\u0002\u00d53\u0003\u0002\u0002",
    "\u0002\u00d5<\u0003\u0002\u0002\u0002\u00d5W\u0003\u0002\u0002\u0002",
    "\u00d5c\u0003\u0002\u0002\u0002\u00d5l\u0003\u0002\u0002\u0002\u00d5",
    "\u0086\u0003\u0002\u0002\u0002\u00d5\u008f\u0003\u0002\u0002\u0002\u00d5",
    "\u0098\u0003\u0002\u0002\u0002\u00d5\u009d\u0003\u0002\u0002\u0002\u00d5",
    "\u00a8\u0003\u0002\u0002\u0002\u00d5\u00b1\u0003\u0002\u0002\u0002\u00d5",
    "\u00bb\u0003\u0002\u0002\u0002\u00d5\u00c5\u0003\u0002\u0002\u0002\u00d5",
    "\u00cd\u0003\u0002\u0002\u0002\u00d5\u00ce\u0003\u0002\u0002\u0002\u00d5",
    "\u00cf\u0003\u0002\u0002\u0002\u00d5\u00d0\u0003\u0002\u0002\u0002\u00d5",
    "\u00d1\u0003\u0002\u0002\u0002\u00d5\u00d2\u0003\u0002\u0002\u0002\u00d5",
    "\u00d3\u0003\u0002\u0002\u0002\u00d6\u0005\u0003\u0002\u0002\u0002\u00d7",
    "\u00d8\u0007\u0003\u0002\u0002\u00d8\u00dc\u0005\b\u0005\u0002\u00d9",
    "\u00db\u0005\n\u0006\u0002\u00da\u00d9\u0003\u0002\u0002\u0002\u00db",
    "\u00de\u0003\u0002\u0002\u0002\u00dc\u00da\u0003\u0002\u0002\u0002\u00dc",
    "\u00dd\u0003\u0002\u0002\u0002\u00dd\u00df\u0003\u0002\u0002\u0002\u00de",
    "\u00dc\u0003\u0002\u0002\u0002\u00df\u00e0\u0007\u0004\u0002\u0002\u00e0",
    "\u0007\u0003\u0002\u0002\u0002\u00e1\u00e2\u0005\u0004\u0003\u0002\u00e2",
    "\t\u0003\u0002\u0002\u0002\u00e3\u00e4\u0005\u0004\u0003\u0002\u00e4",
    "\u000b\u0003\u0002\u0002\u0002\u00e5\u00e6\u0007\u0019\u0002\u0002\u00e6",
    "\r\u0003\u0002\u0002\u0002\u00e7\u00e8\u0007\u0019\u0002\u0002\u00e8",
    "\u000f\u0003\u0002\u0002\u0002\u00e9\u00ea\u0007\u0019\u0002\u0002\u00ea",
    "\u0011\u0003\u0002\u0002\u0002\u00eb\u00ec\u0007\u0019\u0002\u0002\u00ec",
    "\u0013\u0003\u0002\u0002\u0002\u00ed\u00ee\u0005\u0004\u0003\u0002\u00ee",
    "\u0015\u0003\u0002\u0002\u0002\u00ef\u00f0\u0005\u0004\u0003\u0002\u00f0",
    "\u0017\u0003\u0002\u0002\u0002\u00f1\u00f2\u0005\u0004\u0003\u0002\u00f2",
    "\u0019\u0003\u0002\u0002\u0002\u00f3\u00f4\u0005\u0004\u0003\u0002\u00f4",
    "\u001b\u0003\u0002\u0002\u0002\u00f5\u00f6\u0005\u0004\u0003\u0002\u00f6",
    "\u001d\u0003\u0002\u0002\u0002\u00f7\u00f8\u0007\u0019\u0002\u0002\u00f8",
    "\u00f9\u0005\u0004\u0003\u0002\u00f9\u001f\u0003\u0002\u0002\u0002\u00fa",
    "\u00fb\u0005\u0004\u0003\u0002\u00fb!\u0003\u0002\u0002\u0002\u00fc",
    "\u00fd\u0005\u0004\u0003\u0002\u00fd#\u0003\u0002\u0002\u0002\u0018",
    "\'/8CJMS^hry|\u0082\u008b\u0094\u00a3\u00ad\u00b7\u00bf\u00c9\u00d5",
    "\u00dc"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "'('", "')'", "' / '", "'.'", "'''" ];

var symbolicNames = [ null, null, null, null, null, null, "AND", "COND", 
                      "DEFUN", "FOREACH", "IF", "LAMBDA", "OR", "PROGN", 
                      "QUOTE", "REPEAT", "SETQ", "WHILE", "NIL", "TRU", 
                      "INT", "REAL", "STR", "ID", "INLINE_COMMENT", "LINE_COMMENT", 
                      "NEWLINE", "WHITESPACE" ];

var ruleNames =  [ "file", "expr", "condTestResult", "condTest", "condResult", 
                   "funName", "funParam", "funLocal", "foreachName", "foreachList", 
                   "ifTest", "ifThen", "ifElse", "repeatNum", "setqNameExpr", 
                   "whileTest", "listExpr" ];

function VeLispParser (input) {
	antlr4.Parser.call(this, input);
    this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
    this.ruleNames = ruleNames;
    this.literalNames = literalNames;
    this.symbolicNames = symbolicNames;
    return this;
}

VeLispParser.prototype = Object.create(antlr4.Parser.prototype);
VeLispParser.prototype.constructor = VeLispParser;

Object.defineProperty(VeLispParser.prototype, "atn", {
	get : function() {
		return atn;
	}
});

VeLispParser.EOF = antlr4.Token.EOF;
VeLispParser.T__0 = 1;
VeLispParser.T__1 = 2;
VeLispParser.T__2 = 3;
VeLispParser.T__3 = 4;
VeLispParser.T__4 = 5;
VeLispParser.AND = 6;
VeLispParser.COND = 7;
VeLispParser.DEFUN = 8;
VeLispParser.FOREACH = 9;
VeLispParser.IF = 10;
VeLispParser.LAMBDA = 11;
VeLispParser.OR = 12;
VeLispParser.PROGN = 13;
VeLispParser.QUOTE = 14;
VeLispParser.REPEAT = 15;
VeLispParser.SETQ = 16;
VeLispParser.WHILE = 17;
VeLispParser.NIL = 18;
VeLispParser.TRU = 19;
VeLispParser.INT = 20;
VeLispParser.REAL = 21;
VeLispParser.STR = 22;
VeLispParser.ID = 23;
VeLispParser.INLINE_COMMENT = 24;
VeLispParser.LINE_COMMENT = 25;
VeLispParser.NEWLINE = 26;
VeLispParser.WHITESPACE = 27;

VeLispParser.RULE_file = 0;
VeLispParser.RULE_expr = 1;
VeLispParser.RULE_condTestResult = 2;
VeLispParser.RULE_condTest = 3;
VeLispParser.RULE_condResult = 4;
VeLispParser.RULE_funName = 5;
VeLispParser.RULE_funParam = 6;
VeLispParser.RULE_funLocal = 7;
VeLispParser.RULE_foreachName = 8;
VeLispParser.RULE_foreachList = 9;
VeLispParser.RULE_ifTest = 10;
VeLispParser.RULE_ifThen = 11;
VeLispParser.RULE_ifElse = 12;
VeLispParser.RULE_repeatNum = 13;
VeLispParser.RULE_setqNameExpr = 14;
VeLispParser.RULE_whileTest = 15;
VeLispParser.RULE_listExpr = 16;


function FileContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_file;
    return this;
}

FileContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FileContext.prototype.constructor = FileContext;

FileContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

FileContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitFile(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.FileContext = FileContext;

VeLispParser.prototype.file = function() {

    var localctx = new FileContext(this, this._ctx, this.state);
    this.enterRule(localctx, 0, VeLispParser.RULE_file);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 37;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
            this.state = 34;
            this.expr();
            this.state = 39;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function ExprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_expr;
    return this;
}

ExprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ExprContext.prototype.constructor = ExprContext;


 
ExprContext.prototype.copyFrom = function(ctx) {
    antlr4.ParserRuleContext.prototype.copyFrom.call(this, ctx);
};


function OrContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

OrContext.prototype = Object.create(ExprContext.prototype);
OrContext.prototype.constructor = OrContext;

VeLispParser.OrContext = OrContext;

OrContext.prototype.OR = function() {
    return this.getToken(VeLispParser.OR, 0);
};

OrContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
OrContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitOr(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function PrognContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

PrognContext.prototype = Object.create(ExprContext.prototype);
PrognContext.prototype.constructor = PrognContext;

VeLispParser.PrognContext = PrognContext;

PrognContext.prototype.PROGN = function() {
    return this.getToken(VeLispParser.PROGN, 0);
};

PrognContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
PrognContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitProgn(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function RealContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

RealContext.prototype = Object.create(ExprContext.prototype);
RealContext.prototype.constructor = RealContext;

VeLispParser.RealContext = RealContext;

RealContext.prototype.REAL = function() {
    return this.getToken(VeLispParser.REAL, 0);
};
RealContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitReal(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function TickContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

TickContext.prototype = Object.create(ExprContext.prototype);
TickContext.prototype.constructor = TickContext;

VeLispParser.TickContext = TickContext;

TickContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};
TickContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitTick(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function CondContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

CondContext.prototype = Object.create(ExprContext.prototype);
CondContext.prototype.constructor = CondContext;

VeLispParser.CondContext = CondContext;

CondContext.prototype.COND = function() {
    return this.getToken(VeLispParser.COND, 0);
};

CondContext.prototype.condTestResult = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(CondTestResultContext);
    } else {
        return this.getTypedRuleContext(CondTestResultContext,i);
    }
};
CondContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitCond(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function WhileContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

WhileContext.prototype = Object.create(ExprContext.prototype);
WhileContext.prototype.constructor = WhileContext;

VeLispParser.WhileContext = WhileContext;

WhileContext.prototype.WHILE = function() {
    return this.getToken(VeLispParser.WHILE, 0);
};

WhileContext.prototype.whileTest = function() {
    return this.getTypedRuleContext(WhileTestContext,0);
};

WhileContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
WhileContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitWhile(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function ListContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

ListContext.prototype = Object.create(ExprContext.prototype);
ListContext.prototype.constructor = ListContext;

VeLispParser.ListContext = ListContext;

ListContext.prototype.listExpr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ListExprContext);
    } else {
        return this.getTypedRuleContext(ListExprContext,i);
    }
};
ListContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitList(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function DefunContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

DefunContext.prototype = Object.create(ExprContext.prototype);
DefunContext.prototype.constructor = DefunContext;

VeLispParser.DefunContext = DefunContext;

DefunContext.prototype.DEFUN = function() {
    return this.getToken(VeLispParser.DEFUN, 0);
};

DefunContext.prototype.funName = function() {
    return this.getTypedRuleContext(FunNameContext,0);
};

DefunContext.prototype.funParam = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(FunParamContext);
    } else {
        return this.getTypedRuleContext(FunParamContext,i);
    }
};

DefunContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

DefunContext.prototype.funLocal = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(FunLocalContext);
    } else {
        return this.getTypedRuleContext(FunLocalContext,i);
    }
};
DefunContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitDefun(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function IntContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

IntContext.prototype = Object.create(ExprContext.prototype);
IntContext.prototype.constructor = IntContext;

VeLispParser.IntContext = IntContext;

IntContext.prototype.INT = function() {
    return this.getToken(VeLispParser.INT, 0);
};
IntContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitInt(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function NilContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

NilContext.prototype = Object.create(ExprContext.prototype);
NilContext.prototype.constructor = NilContext;

VeLispParser.NilContext = NilContext;

NilContext.prototype.NIL = function() {
    return this.getToken(VeLispParser.NIL, 0);
};
NilContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitNil(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function StrContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

StrContext.prototype = Object.create(ExprContext.prototype);
StrContext.prototype.constructor = StrContext;

VeLispParser.StrContext = StrContext;

StrContext.prototype.STR = function() {
    return this.getToken(VeLispParser.STR, 0);
};
StrContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitStr(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function ForeachContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

ForeachContext.prototype = Object.create(ExprContext.prototype);
ForeachContext.prototype.constructor = ForeachContext;

VeLispParser.ForeachContext = ForeachContext;

ForeachContext.prototype.FOREACH = function() {
    return this.getToken(VeLispParser.FOREACH, 0);
};

ForeachContext.prototype.foreachName = function() {
    return this.getTypedRuleContext(ForeachNameContext,0);
};

ForeachContext.prototype.foreachList = function() {
    return this.getTypedRuleContext(ForeachListContext,0);
};

ForeachContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
ForeachContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitForeach(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function LambdaContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

LambdaContext.prototype = Object.create(ExprContext.prototype);
LambdaContext.prototype.constructor = LambdaContext;

VeLispParser.LambdaContext = LambdaContext;

LambdaContext.prototype.LAMBDA = function() {
    return this.getToken(VeLispParser.LAMBDA, 0);
};

LambdaContext.prototype.funParam = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(FunParamContext);
    } else {
        return this.getTypedRuleContext(FunParamContext,i);
    }
};

LambdaContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};

LambdaContext.prototype.funLocal = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(FunLocalContext);
    } else {
        return this.getTypedRuleContext(FunLocalContext,i);
    }
};
LambdaContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitLambda(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function QuoteContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

QuoteContext.prototype = Object.create(ExprContext.prototype);
QuoteContext.prototype.constructor = QuoteContext;

VeLispParser.QuoteContext = QuoteContext;

QuoteContext.prototype.QUOTE = function() {
    return this.getToken(VeLispParser.QUOTE, 0);
};

QuoteContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};
QuoteContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitQuote(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function AndContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

AndContext.prototype = Object.create(ExprContext.prototype);
AndContext.prototype.constructor = AndContext;

VeLispParser.AndContext = AndContext;

AndContext.prototype.AND = function() {
    return this.getToken(VeLispParser.AND, 0);
};

AndContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
AndContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitAnd(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function TruContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

TruContext.prototype = Object.create(ExprContext.prototype);
TruContext.prototype.constructor = TruContext;

VeLispParser.TruContext = TruContext;

TruContext.prototype.TRU = function() {
    return this.getToken(VeLispParser.TRU, 0);
};
TruContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitTru(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function RepeatContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

RepeatContext.prototype = Object.create(ExprContext.prototype);
RepeatContext.prototype.constructor = RepeatContext;

VeLispParser.RepeatContext = RepeatContext;

RepeatContext.prototype.REPEAT = function() {
    return this.getToken(VeLispParser.REPEAT, 0);
};

RepeatContext.prototype.repeatNum = function() {
    return this.getTypedRuleContext(RepeatNumContext,0);
};

RepeatContext.prototype.expr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExprContext);
    } else {
        return this.getTypedRuleContext(ExprContext,i);
    }
};
RepeatContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitRepeat(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function DotListContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

DotListContext.prototype = Object.create(ExprContext.prototype);
DotListContext.prototype.constructor = DotListContext;

VeLispParser.DotListContext = DotListContext;

DotListContext.prototype.listExpr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ListExprContext);
    } else {
        return this.getTypedRuleContext(ListExprContext,i);
    }
};
DotListContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitDotList(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function SetQContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

SetQContext.prototype = Object.create(ExprContext.prototype);
SetQContext.prototype.constructor = SetQContext;

VeLispParser.SetQContext = SetQContext;

SetQContext.prototype.SETQ = function() {
    return this.getToken(VeLispParser.SETQ, 0);
};

SetQContext.prototype.setqNameExpr = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(SetqNameExprContext);
    } else {
        return this.getTypedRuleContext(SetqNameExprContext,i);
    }
};
SetQContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitSetQ(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function IdContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

IdContext.prototype = Object.create(ExprContext.prototype);
IdContext.prototype.constructor = IdContext;

VeLispParser.IdContext = IdContext;

IdContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};
IdContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitId(this);
    } else {
        return visitor.visitChildren(this);
    }
};


function IfContext(parser, ctx) {
	ExprContext.call(this, parser);
    ExprContext.prototype.copyFrom.call(this, ctx);
    return this;
}

IfContext.prototype = Object.create(ExprContext.prototype);
IfContext.prototype.constructor = IfContext;

VeLispParser.IfContext = IfContext;

IfContext.prototype.IF = function() {
    return this.getToken(VeLispParser.IF, 0);
};

IfContext.prototype.ifTest = function() {
    return this.getTypedRuleContext(IfTestContext,0);
};

IfContext.prototype.ifThen = function() {
    return this.getTypedRuleContext(IfThenContext,0);
};

IfContext.prototype.ifElse = function() {
    return this.getTypedRuleContext(IfElseContext,0);
};
IfContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitIf(this);
    } else {
        return visitor.visitChildren(this);
    }
};



VeLispParser.ExprContext = ExprContext;

VeLispParser.prototype.expr = function() {

    var localctx = new ExprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 2, VeLispParser.RULE_expr);
    var _la = 0; // Token type
    try {
        this.state = 211;
        this._errHandler.sync(this);
        var la_ = this._interp.adaptivePredict(this._input,20,this._ctx);
        switch(la_) {
        case 1:
            localctx = new AndContext(this, localctx);
            this.enterOuterAlt(localctx, 1);
            this.state = 40;
            this.match(VeLispParser.T__0);
            this.state = 41;
            this.match(VeLispParser.AND);
            this.state = 45;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 42;
                this.expr();
                this.state = 47;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 48;
            this.match(VeLispParser.T__1);
            break;

        case 2:
            localctx = new CondContext(this, localctx);
            this.enterOuterAlt(localctx, 2);
            this.state = 49;
            this.match(VeLispParser.T__0);
            this.state = 50;
            this.match(VeLispParser.COND);
            this.state = 54;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeLispParser.T__0) {
                this.state = 51;
                this.condTestResult();
                this.state = 56;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 57;
            this.match(VeLispParser.T__1);
            break;

        case 3:
            localctx = new DefunContext(this, localctx);
            this.enterOuterAlt(localctx, 3);
            this.state = 58;
            this.match(VeLispParser.T__0);
            this.state = 59;
            this.match(VeLispParser.DEFUN);
            this.state = 60;
            this.funName();
            this.state = 61;
            this.match(VeLispParser.T__0);
            this.state = 65;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeLispParser.ID) {
                this.state = 62;
                this.funParam();
                this.state = 67;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 75;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeLispParser.T__2) {
                this.state = 68;
                this.match(VeLispParser.T__2);
                this.state = 72;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
                while(_la===VeLispParser.ID) {
                    this.state = 69;
                    this.funLocal();
                    this.state = 74;
                    this._errHandler.sync(this);
                    _la = this._input.LA(1);
                }
            }

            this.state = 77;
            this.match(VeLispParser.T__1);
            this.state = 79; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 78;
                this.expr();
                this.state = 81; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
            this.state = 83;
            this.match(VeLispParser.T__1);
            break;

        case 4:
            localctx = new ForeachContext(this, localctx);
            this.enterOuterAlt(localctx, 4);
            this.state = 85;
            this.match(VeLispParser.T__0);
            this.state = 86;
            this.match(VeLispParser.FOREACH);
            this.state = 87;
            this.foreachName();
            this.state = 88;
            this.foreachList();
            this.state = 92;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 89;
                this.expr();
                this.state = 94;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 95;
            this.match(VeLispParser.T__1);
            break;

        case 5:
            localctx = new IfContext(this, localctx);
            this.enterOuterAlt(localctx, 5);
            this.state = 97;
            this.match(VeLispParser.T__0);
            this.state = 98;
            this.match(VeLispParser.IF);
            this.state = 99;
            this.ifTest();
            this.state = 100;
            this.ifThen();
            this.state = 102;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 101;
                this.ifElse();
            }

            this.state = 104;
            this.match(VeLispParser.T__1);
            break;

        case 6:
            localctx = new LambdaContext(this, localctx);
            this.enterOuterAlt(localctx, 6);
            this.state = 106;
            this.match(VeLispParser.T__0);
            this.state = 107;
            this.match(VeLispParser.LAMBDA);
            this.state = 108;
            this.match(VeLispParser.T__0);
            this.state = 112;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeLispParser.ID) {
                this.state = 109;
                this.funParam();
                this.state = 114;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 122;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===VeLispParser.T__2) {
                this.state = 115;
                this.match(VeLispParser.T__2);
                this.state = 119;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
                while(_la===VeLispParser.ID) {
                    this.state = 116;
                    this.funLocal();
                    this.state = 121;
                    this._errHandler.sync(this);
                    _la = this._input.LA(1);
                }
            }

            this.state = 124;
            this.match(VeLispParser.T__1);
            this.state = 126; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 125;
                this.expr();
                this.state = 128; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
            this.state = 130;
            this.match(VeLispParser.T__1);
            break;

        case 7:
            localctx = new OrContext(this, localctx);
            this.enterOuterAlt(localctx, 7);
            this.state = 132;
            this.match(VeLispParser.T__0);
            this.state = 133;
            this.match(VeLispParser.OR);
            this.state = 137;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 134;
                this.expr();
                this.state = 139;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 140;
            this.match(VeLispParser.T__1);
            break;

        case 8:
            localctx = new PrognContext(this, localctx);
            this.enterOuterAlt(localctx, 8);
            this.state = 141;
            this.match(VeLispParser.T__0);
            this.state = 142;
            this.match(VeLispParser.PROGN);
            this.state = 146;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 143;
                this.expr();
                this.state = 148;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 149;
            this.match(VeLispParser.T__1);
            break;

        case 9:
            localctx = new QuoteContext(this, localctx);
            this.enterOuterAlt(localctx, 9);
            this.state = 150;
            this.match(VeLispParser.T__0);
            this.state = 151;
            this.match(VeLispParser.QUOTE);
            this.state = 152;
            this.expr();
            this.state = 153;
            this.match(VeLispParser.T__1);
            break;

        case 10:
            localctx = new RepeatContext(this, localctx);
            this.enterOuterAlt(localctx, 10);
            this.state = 155;
            this.match(VeLispParser.T__0);
            this.state = 156;
            this.match(VeLispParser.REPEAT);
            this.state = 157;
            this.repeatNum();
            this.state = 161;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 158;
                this.expr();
                this.state = 163;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 164;
            this.match(VeLispParser.T__1);
            break;

        case 11:
            localctx = new SetQContext(this, localctx);
            this.enterOuterAlt(localctx, 11);
            this.state = 166;
            this.match(VeLispParser.T__0);
            this.state = 167;
            this.match(VeLispParser.SETQ);
            this.state = 171;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while(_la===VeLispParser.ID) {
                this.state = 168;
                this.setqNameExpr();
                this.state = 173;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 174;
            this.match(VeLispParser.T__1);
            break;

        case 12:
            localctx = new WhileContext(this, localctx);
            this.enterOuterAlt(localctx, 12);
            this.state = 175;
            this.match(VeLispParser.T__0);
            this.state = 176;
            this.match(VeLispParser.WHILE);
            this.state = 177;
            this.whileTest();
            this.state = 179; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 178;
                this.expr();
                this.state = 181; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
            this.state = 183;
            this.match(VeLispParser.T__1);
            break;

        case 13:
            localctx = new DotListContext(this, localctx);
            this.enterOuterAlt(localctx, 13);
            this.state = 185;
            this.match(VeLispParser.T__0);
            this.state = 187; 
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            do {
                this.state = 186;
                this.listExpr();
                this.state = 189; 
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
            this.state = 191;
            this.match(VeLispParser.T__3);
            this.state = 192;
            this.listExpr();
            this.state = 193;
            this.match(VeLispParser.T__1);
            break;

        case 14:
            localctx = new ListContext(this, localctx);
            this.enterOuterAlt(localctx, 14);
            this.state = 195;
            this.match(VeLispParser.T__0);
            this.state = 199;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
                this.state = 196;
                this.listExpr();
                this.state = 201;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
            }
            this.state = 202;
            this.match(VeLispParser.T__1);
            break;

        case 15:
            localctx = new NilContext(this, localctx);
            this.enterOuterAlt(localctx, 15);
            this.state = 203;
            this.match(VeLispParser.NIL);
            break;

        case 16:
            localctx = new TruContext(this, localctx);
            this.enterOuterAlt(localctx, 16);
            this.state = 204;
            this.match(VeLispParser.TRU);
            break;

        case 17:
            localctx = new IntContext(this, localctx);
            this.enterOuterAlt(localctx, 17);
            this.state = 205;
            this.match(VeLispParser.INT);
            break;

        case 18:
            localctx = new RealContext(this, localctx);
            this.enterOuterAlt(localctx, 18);
            this.state = 206;
            this.match(VeLispParser.REAL);
            break;

        case 19:
            localctx = new StrContext(this, localctx);
            this.enterOuterAlt(localctx, 19);
            this.state = 207;
            this.match(VeLispParser.STR);
            break;

        case 20:
            localctx = new IdContext(this, localctx);
            this.enterOuterAlt(localctx, 20);
            this.state = 208;
            this.match(VeLispParser.ID);
            break;

        case 21:
            localctx = new TickContext(this, localctx);
            this.enterOuterAlt(localctx, 21);
            this.state = 209;
            this.match(VeLispParser.T__4);
            this.state = 210;
            this.expr();
            break;

        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function CondTestResultContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_condTestResult;
    return this;
}

CondTestResultContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
CondTestResultContext.prototype.constructor = CondTestResultContext;

CondTestResultContext.prototype.condTest = function() {
    return this.getTypedRuleContext(CondTestContext,0);
};

CondTestResultContext.prototype.condResult = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(CondResultContext);
    } else {
        return this.getTypedRuleContext(CondResultContext,i);
    }
};

CondTestResultContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitCondTestResult(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.CondTestResultContext = CondTestResultContext;

VeLispParser.prototype.condTestResult = function() {

    var localctx = new CondTestResultContext(this, this._ctx, this.state);
    this.enterRule(localctx, 4, VeLispParser.RULE_condTestResult);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 213;
        this.match(VeLispParser.T__0);
        this.state = 214;
        this.condTest();
        this.state = 218;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
            this.state = 215;
            this.condResult();
            this.state = 220;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 221;
        this.match(VeLispParser.T__1);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function CondTestContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_condTest;
    return this;
}

CondTestContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
CondTestContext.prototype.constructor = CondTestContext;

CondTestContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

CondTestContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitCondTest(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.CondTestContext = CondTestContext;

VeLispParser.prototype.condTest = function() {

    var localctx = new CondTestContext(this, this._ctx, this.state);
    this.enterRule(localctx, 6, VeLispParser.RULE_condTest);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 223;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function CondResultContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_condResult;
    return this;
}

CondResultContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
CondResultContext.prototype.constructor = CondResultContext;

CondResultContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

CondResultContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitCondResult(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.CondResultContext = CondResultContext;

VeLispParser.prototype.condResult = function() {

    var localctx = new CondResultContext(this, this._ctx, this.state);
    this.enterRule(localctx, 8, VeLispParser.RULE_condResult);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 225;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function FunNameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_funName;
    return this;
}

FunNameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FunNameContext.prototype.constructor = FunNameContext;

FunNameContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

FunNameContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitFunName(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.FunNameContext = FunNameContext;

VeLispParser.prototype.funName = function() {

    var localctx = new FunNameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 10, VeLispParser.RULE_funName);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 227;
        this.match(VeLispParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function FunParamContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_funParam;
    return this;
}

FunParamContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FunParamContext.prototype.constructor = FunParamContext;

FunParamContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

FunParamContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitFunParam(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.FunParamContext = FunParamContext;

VeLispParser.prototype.funParam = function() {

    var localctx = new FunParamContext(this, this._ctx, this.state);
    this.enterRule(localctx, 12, VeLispParser.RULE_funParam);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 229;
        this.match(VeLispParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function FunLocalContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_funLocal;
    return this;
}

FunLocalContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
FunLocalContext.prototype.constructor = FunLocalContext;

FunLocalContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

FunLocalContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitFunLocal(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.FunLocalContext = FunLocalContext;

VeLispParser.prototype.funLocal = function() {

    var localctx = new FunLocalContext(this, this._ctx, this.state);
    this.enterRule(localctx, 14, VeLispParser.RULE_funLocal);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 231;
        this.match(VeLispParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function ForeachNameContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_foreachName;
    return this;
}

ForeachNameContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ForeachNameContext.prototype.constructor = ForeachNameContext;

ForeachNameContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

ForeachNameContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitForeachName(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.ForeachNameContext = ForeachNameContext;

VeLispParser.prototype.foreachName = function() {

    var localctx = new ForeachNameContext(this, this._ctx, this.state);
    this.enterRule(localctx, 16, VeLispParser.RULE_foreachName);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 233;
        this.match(VeLispParser.ID);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function ForeachListContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_foreachList;
    return this;
}

ForeachListContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ForeachListContext.prototype.constructor = ForeachListContext;

ForeachListContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

ForeachListContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitForeachList(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.ForeachListContext = ForeachListContext;

VeLispParser.prototype.foreachList = function() {

    var localctx = new ForeachListContext(this, this._ctx, this.state);
    this.enterRule(localctx, 18, VeLispParser.RULE_foreachList);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 235;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function IfTestContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_ifTest;
    return this;
}

IfTestContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IfTestContext.prototype.constructor = IfTestContext;

IfTestContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

IfTestContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitIfTest(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.IfTestContext = IfTestContext;

VeLispParser.prototype.ifTest = function() {

    var localctx = new IfTestContext(this, this._ctx, this.state);
    this.enterRule(localctx, 20, VeLispParser.RULE_ifTest);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 237;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function IfThenContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_ifThen;
    return this;
}

IfThenContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IfThenContext.prototype.constructor = IfThenContext;

IfThenContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

IfThenContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitIfThen(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.IfThenContext = IfThenContext;

VeLispParser.prototype.ifThen = function() {

    var localctx = new IfThenContext(this, this._ctx, this.state);
    this.enterRule(localctx, 22, VeLispParser.RULE_ifThen);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 239;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function IfElseContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_ifElse;
    return this;
}

IfElseContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
IfElseContext.prototype.constructor = IfElseContext;

IfElseContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

IfElseContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitIfElse(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.IfElseContext = IfElseContext;

VeLispParser.prototype.ifElse = function() {

    var localctx = new IfElseContext(this, this._ctx, this.state);
    this.enterRule(localctx, 24, VeLispParser.RULE_ifElse);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 241;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function RepeatNumContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_repeatNum;
    return this;
}

RepeatNumContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
RepeatNumContext.prototype.constructor = RepeatNumContext;

RepeatNumContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

RepeatNumContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitRepeatNum(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.RepeatNumContext = RepeatNumContext;

VeLispParser.prototype.repeatNum = function() {

    var localctx = new RepeatNumContext(this, this._ctx, this.state);
    this.enterRule(localctx, 26, VeLispParser.RULE_repeatNum);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 243;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function SetqNameExprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_setqNameExpr;
    return this;
}

SetqNameExprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
SetqNameExprContext.prototype.constructor = SetqNameExprContext;

SetqNameExprContext.prototype.ID = function() {
    return this.getToken(VeLispParser.ID, 0);
};

SetqNameExprContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

SetqNameExprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitSetqNameExpr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.SetqNameExprContext = SetqNameExprContext;

VeLispParser.prototype.setqNameExpr = function() {

    var localctx = new SetqNameExprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 28, VeLispParser.RULE_setqNameExpr);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 245;
        this.match(VeLispParser.ID);
        this.state = 246;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function WhileTestContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_whileTest;
    return this;
}

WhileTestContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
WhileTestContext.prototype.constructor = WhileTestContext;

WhileTestContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

WhileTestContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitWhileTest(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.WhileTestContext = WhileTestContext;

VeLispParser.prototype.whileTest = function() {

    var localctx = new WhileTestContext(this, this._ctx, this.state);
    this.enterRule(localctx, 30, VeLispParser.RULE_whileTest);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 248;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


function ListExprContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = VeLispParser.RULE_listExpr;
    return this;
}

ListExprContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ListExprContext.prototype.constructor = ListExprContext;

ListExprContext.prototype.expr = function() {
    return this.getTypedRuleContext(ExprContext,0);
};

ListExprContext.prototype.accept = function(visitor) {
    if ( visitor instanceof VeLispVisitor ) {
        return visitor.visitListExpr(this);
    } else {
        return visitor.visitChildren(this);
    }
};




VeLispParser.ListExprContext = ListExprContext;

VeLispParser.prototype.listExpr = function() {

    var localctx = new ListExprContext(this, this._ctx, this.state);
    this.enterRule(localctx, 32, VeLispParser.RULE_listExpr);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 250;
        this.expr();
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


exports.VeLispParser = VeLispParser;
