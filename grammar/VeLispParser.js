// Generated from grammar/VeLisp.g4 by ANTLR 4.9.3
// jshint ignore: start
import antlr4 from 'antlr4';
import VeLispVisitor from './VeLispVisitor.js';


const serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786",
    "\u5964\u0003\u001e\u0104\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004",
    "\u0004\t\u0004\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t",
    "\u0007\u0004\b\t\b\u0004\t\t\t\u0004\n\t\n\u0004\u000b\t\u000b\u0004",
    "\f\t\f\u0004\r\t\r\u0004\u000e\t\u000e\u0004\u000f\t\u000f\u0004\u0010",
    "\t\u0010\u0004\u0011\t\u0011\u0004\u0012\t\u0012\u0003\u0002\u0007\u0002",
    "&\n\u0002\f\u0002\u000e\u0002)\u000b\u0002\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0007\u0003.\n\u0003\f\u0003\u000e\u00031\u000b\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u00037\n\u0003\f\u0003\u000e",
    "\u0003:\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0007\u0003B\n\u0003\f\u0003\u000e\u0003E\u000b",
    "\u0003\u0003\u0003\u0003\u0003\u0007\u0003I\n\u0003\f\u0003\u000e\u0003",
    "L\u000b\u0003\u0005\u0003N\n\u0003\u0003\u0003\u0003\u0003\u0006\u0003",
    "R\n\u0003\r\u0003\u000e\u0003S\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003]\n\u0003\f\u0003",
    "\u000e\u0003`\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0005\u0003n\n\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003v",
    "\n\u0003\f\u0003\u000e\u0003y\u000b\u0003\u0003\u0003\u0003\u0003\u0007",
    "\u0003}\n\u0003\f\u0003\u000e\u0003\u0080\u000b\u0003\u0005\u0003\u0082",
    "\n\u0003\u0003\u0003\u0003\u0003\u0006\u0003\u0086\n\u0003\r\u0003\u000e",
    "\u0003\u0087\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0007\u0003\u008f\n\u0003\f\u0003\u000e\u0003\u0092\u000b\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007\u0003\u0098\n\u0003",
    "\f\u0003\u000e\u0003\u009b\u000b\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0007\u0003\u00a7\n\u0003\f\u0003\u000e\u0003\u00aa",
    "\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0007\u0003\u00b1\n\u0003\f\u0003\u000e\u0003\u00b4\u000b\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0006\u0003\u00bb",
    "\n\u0003\r\u0003\u000e\u0003\u00bc\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0006\u0003\u00c3\n\u0003\r\u0003\u000e\u0003\u00c4\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0007",
    "\u0003\u00cd\n\u0003\f\u0003\u000e\u0003\u00d0\u000b\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0005\u0003\u00db\n\u0003\u0003\u0004\u0003",
    "\u0004\u0003\u0004\u0007\u0004\u00e0\n\u0004\f\u0004\u000e\u0004\u00e3",
    "\u000b\u0004\u0003\u0004\u0003\u0004\u0003\u0005\u0003\u0005\u0003\u0006",
    "\u0003\u0006\u0003\u0007\u0003\u0007\u0003\b\u0003\b\u0003\t\u0003\t",
    "\u0003\n\u0003\n\u0003\u000b\u0003\u000b\u0003\f\u0003\f\u0003\r\u0003",
    "\r\u0003\u000e\u0003\u000e\u0003\u000f\u0003\u000f\u0003\u0010\u0003",
    "\u0010\u0003\u0010\u0003\u0011\u0003\u0011\u0003\u0012\u0003\u0012\u0003",
    "\u0012\u0002\u0002\u0013\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014",
    "\u0016\u0018\u001a\u001c\u001e \"\u0002\u0002\u0002\u011c\u0002\'\u0003",
    "\u0002\u0002\u0002\u0004\u00da\u0003\u0002\u0002\u0002\u0006\u00dc\u0003",
    "\u0002\u0002\u0002\b\u00e6\u0003\u0002\u0002\u0002\n\u00e8\u0003\u0002",
    "\u0002\u0002\f\u00ea\u0003\u0002\u0002\u0002\u000e\u00ec\u0003\u0002",
    "\u0002\u0002\u0010\u00ee\u0003\u0002\u0002\u0002\u0012\u00f0\u0003\u0002",
    "\u0002\u0002\u0014\u00f2\u0003\u0002\u0002\u0002\u0016\u00f4\u0003\u0002",
    "\u0002\u0002\u0018\u00f6\u0003\u0002\u0002\u0002\u001a\u00f8\u0003\u0002",
    "\u0002\u0002\u001c\u00fa\u0003\u0002\u0002\u0002\u001e\u00fc\u0003\u0002",
    "\u0002\u0002 \u00ff\u0003\u0002\u0002\u0002\"\u0101\u0003\u0002\u0002",
    "\u0002$&\u0005\u0004\u0003\u0002%$\u0003\u0002\u0002\u0002&)\u0003\u0002",
    "\u0002\u0002\'%\u0003\u0002\u0002\u0002\'(\u0003\u0002\u0002\u0002(",
    "\u0003\u0003\u0002\u0002\u0002)\'\u0003\u0002\u0002\u0002*+\u0007\u0003",
    "\u0002\u0002+/\u0007\b\u0002\u0002,.\u0005\u0004\u0003\u0002-,\u0003",
    "\u0002\u0002\u0002.1\u0003\u0002\u0002\u0002/-\u0003\u0002\u0002\u0002",
    "/0\u0003\u0002\u0002\u000202\u0003\u0002\u0002\u00021/\u0003\u0002\u0002",
    "\u00022\u00db\u0007\u0004\u0002\u000234\u0007\u0003\u0002\u000248\u0007",
    "\t\u0002\u000257\u0005\u0006\u0004\u000265\u0003\u0002\u0002\u00027",
    ":\u0003\u0002\u0002\u000286\u0003\u0002\u0002\u000289\u0003\u0002\u0002",
    "\u00029;\u0003\u0002\u0002\u0002:8\u0003\u0002\u0002\u0002;\u00db\u0007",
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
    "\u0002V\u00db\u0003\u0002\u0002\u0002WX\u0007\u0003\u0002\u0002XY\u0007",
    "\u000b\u0002\u0002YZ\u0005\u0012\n\u0002Z^\u0005\u0014\u000b\u0002[",
    "]\u0005\u0004\u0003\u0002\\[\u0003\u0002\u0002\u0002]`\u0003\u0002\u0002",
    "\u0002^\\\u0003\u0002\u0002\u0002^_\u0003\u0002\u0002\u0002_a\u0003",
    "\u0002\u0002\u0002`^\u0003\u0002\u0002\u0002ab\u0007\u0004\u0002\u0002",
    "b\u00db\u0003\u0002\u0002\u0002cd\u0007\u0003\u0002\u0002de\u0007\f",
    "\u0002\u0002ef\u0005\u0004\u0003\u0002fg\u0007\u0004\u0002\u0002g\u00db",
    "\u0003\u0002\u0002\u0002hi\u0007\u0003\u0002\u0002ij\u0007\r\u0002\u0002",
    "jk\u0005\u0016\f\u0002km\u0005\u0018\r\u0002ln\u0005\u001a\u000e\u0002",
    "ml\u0003\u0002\u0002\u0002mn\u0003\u0002\u0002\u0002no\u0003\u0002\u0002",
    "\u0002op\u0007\u0004\u0002\u0002p\u00db\u0003\u0002\u0002\u0002qr\u0007",
    "\u0003\u0002\u0002rs\u0007\u000e\u0002\u0002sw\u0007\u0003\u0002\u0002",
    "tv\u0005\u000e\b\u0002ut\u0003\u0002\u0002\u0002vy\u0003\u0002\u0002",
    "\u0002wu\u0003\u0002\u0002\u0002wx\u0003\u0002\u0002\u0002x\u0081\u0003",
    "\u0002\u0002\u0002yw\u0003\u0002\u0002\u0002z~\u0007\u0005\u0002\u0002",
    "{}\u0005\u0010\t\u0002|{\u0003\u0002\u0002\u0002}\u0080\u0003\u0002",
    "\u0002\u0002~|\u0003\u0002\u0002\u0002~\u007f\u0003\u0002\u0002\u0002",
    "\u007f\u0082\u0003\u0002\u0002\u0002\u0080~\u0003\u0002\u0002\u0002",
    "\u0081z\u0003\u0002\u0002\u0002\u0081\u0082\u0003\u0002\u0002\u0002",
    "\u0082\u0083\u0003\u0002\u0002\u0002\u0083\u0085\u0007\u0004\u0002\u0002",
    "\u0084\u0086\u0005\u0004\u0003\u0002\u0085\u0084\u0003\u0002\u0002\u0002",
    "\u0086\u0087\u0003\u0002\u0002\u0002\u0087\u0085\u0003\u0002\u0002\u0002",
    "\u0087\u0088\u0003\u0002\u0002\u0002\u0088\u0089\u0003\u0002\u0002\u0002",
    "\u0089\u008a\u0007\u0004\u0002\u0002\u008a\u00db\u0003\u0002\u0002\u0002",
    "\u008b\u008c\u0007\u0003\u0002\u0002\u008c\u0090\u0007\u000f\u0002\u0002",
    "\u008d\u008f\u0005\u0004\u0003\u0002\u008e\u008d\u0003\u0002\u0002\u0002",
    "\u008f\u0092\u0003\u0002\u0002\u0002\u0090\u008e\u0003\u0002\u0002\u0002",
    "\u0090\u0091\u0003\u0002\u0002\u0002\u0091\u0093\u0003\u0002\u0002\u0002",
    "\u0092\u0090\u0003\u0002\u0002\u0002\u0093\u00db\u0007\u0004\u0002\u0002",
    "\u0094\u0095\u0007\u0003\u0002\u0002\u0095\u0099\u0007\u0010\u0002\u0002",
    "\u0096\u0098\u0005\u0004\u0003\u0002\u0097\u0096\u0003\u0002\u0002\u0002",
    "\u0098\u009b\u0003\u0002\u0002\u0002\u0099\u0097\u0003\u0002\u0002\u0002",
    "\u0099\u009a\u0003\u0002\u0002\u0002\u009a\u009c\u0003\u0002\u0002\u0002",
    "\u009b\u0099\u0003\u0002\u0002\u0002\u009c\u00db\u0007\u0004\u0002\u0002",
    "\u009d\u009e\u0007\u0003\u0002\u0002\u009e\u009f\u0007\u0011\u0002\u0002",
    "\u009f\u00a0\u0005\u0004\u0003\u0002\u00a0\u00a1\u0007\u0004\u0002\u0002",
    "\u00a1\u00db\u0003\u0002\u0002\u0002\u00a2\u00a3\u0007\u0003\u0002\u0002",
    "\u00a3\u00a4\u0007\u0012\u0002\u0002\u00a4\u00a8\u0005\u001c\u000f\u0002",
    "\u00a5\u00a7\u0005\u0004\u0003\u0002\u00a6\u00a5\u0003\u0002\u0002\u0002",
    "\u00a7\u00aa\u0003\u0002\u0002\u0002\u00a8\u00a6\u0003\u0002\u0002\u0002",
    "\u00a8\u00a9\u0003\u0002\u0002\u0002\u00a9\u00ab\u0003\u0002\u0002\u0002",
    "\u00aa\u00a8\u0003\u0002\u0002\u0002\u00ab\u00ac\u0007\u0004\u0002\u0002",
    "\u00ac\u00db\u0003\u0002\u0002\u0002\u00ad\u00ae\u0007\u0003\u0002\u0002",
    "\u00ae\u00b2\u0007\u0013\u0002\u0002\u00af\u00b1\u0005\u001e\u0010\u0002",
    "\u00b0\u00af\u0003\u0002\u0002\u0002\u00b1\u00b4\u0003\u0002\u0002\u0002",
    "\u00b2\u00b0\u0003\u0002\u0002\u0002\u00b2\u00b3\u0003\u0002\u0002\u0002",
    "\u00b3\u00b5\u0003\u0002\u0002\u0002\u00b4\u00b2\u0003\u0002\u0002\u0002",
    "\u00b5\u00db\u0007\u0004\u0002\u0002\u00b6\u00b7\u0007\u0003\u0002\u0002",
    "\u00b7\u00b8\u0007\u0014\u0002\u0002\u00b8\u00ba\u0005 \u0011\u0002",
    "\u00b9\u00bb\u0005\u0004\u0003\u0002\u00ba\u00b9\u0003\u0002\u0002\u0002",
    "\u00bb\u00bc\u0003\u0002\u0002\u0002\u00bc\u00ba\u0003\u0002\u0002\u0002",
    "\u00bc\u00bd\u0003\u0002\u0002\u0002\u00bd\u00be\u0003\u0002\u0002\u0002",
    "\u00be\u00bf\u0007\u0004\u0002\u0002\u00bf\u00db\u0003\u0002\u0002\u0002",
    "\u00c0\u00c2\u0007\u0003\u0002\u0002\u00c1\u00c3\u0005\"\u0012\u0002",
    "\u00c2\u00c1\u0003\u0002\u0002\u0002\u00c3\u00c4\u0003\u0002\u0002\u0002",
    "\u00c4\u00c2\u0003\u0002\u0002\u0002\u00c4\u00c5\u0003\u0002\u0002\u0002",
    "\u00c5\u00c6\u0003\u0002\u0002\u0002\u00c6\u00c7\u0007\u0006\u0002\u0002",
    "\u00c7\u00c8\u0005\"\u0012\u0002\u00c8\u00c9\u0007\u0004\u0002\u0002",
    "\u00c9\u00db\u0003\u0002\u0002\u0002\u00ca\u00ce\u0007\u0003\u0002\u0002",
    "\u00cb\u00cd\u0005\"\u0012\u0002\u00cc\u00cb\u0003\u0002\u0002\u0002",
    "\u00cd\u00d0\u0003\u0002\u0002\u0002\u00ce\u00cc\u0003\u0002\u0002\u0002",
    "\u00ce\u00cf\u0003\u0002\u0002\u0002\u00cf\u00d1\u0003\u0002\u0002\u0002",
    "\u00d0\u00ce\u0003\u0002\u0002\u0002\u00d1\u00db\u0007\u0004\u0002\u0002",
    "\u00d2\u00db\u0007\u0015\u0002\u0002\u00d3\u00db\u0007\u0016\u0002\u0002",
    "\u00d4\u00db\u0007\u0017\u0002\u0002\u00d5\u00db\u0007\u0018\u0002\u0002",
    "\u00d6\u00db\u0007\u0019\u0002\u0002\u00d7\u00db\u0007\u001a\u0002\u0002",
    "\u00d8\u00d9\u0007\u0007\u0002\u0002\u00d9\u00db\u0005\u0004\u0003\u0002",
    "\u00da*\u0003\u0002\u0002\u0002\u00da3\u0003\u0002\u0002\u0002\u00da",
    "<\u0003\u0002\u0002\u0002\u00daW\u0003\u0002\u0002\u0002\u00dac\u0003",
    "\u0002\u0002\u0002\u00dah\u0003\u0002\u0002\u0002\u00daq\u0003\u0002",
    "\u0002\u0002\u00da\u008b\u0003\u0002\u0002\u0002\u00da\u0094\u0003\u0002",
    "\u0002\u0002\u00da\u009d\u0003\u0002\u0002\u0002\u00da\u00a2\u0003\u0002",
    "\u0002\u0002\u00da\u00ad\u0003\u0002\u0002\u0002\u00da\u00b6\u0003\u0002",
    "\u0002\u0002\u00da\u00c0\u0003\u0002\u0002\u0002\u00da\u00ca\u0003\u0002",
    "\u0002\u0002\u00da\u00d2\u0003\u0002\u0002\u0002\u00da\u00d3\u0003\u0002",
    "\u0002\u0002\u00da\u00d4\u0003\u0002\u0002\u0002\u00da\u00d5\u0003\u0002",
    "\u0002\u0002\u00da\u00d6\u0003\u0002\u0002\u0002\u00da\u00d7\u0003\u0002",
    "\u0002\u0002\u00da\u00d8\u0003\u0002\u0002\u0002\u00db\u0005\u0003\u0002",
    "\u0002\u0002\u00dc\u00dd\u0007\u0003\u0002\u0002\u00dd\u00e1\u0005\b",
    "\u0005\u0002\u00de\u00e0\u0005\n\u0006\u0002\u00df\u00de\u0003\u0002",
    "\u0002\u0002\u00e0\u00e3\u0003\u0002\u0002\u0002\u00e1\u00df\u0003\u0002",
    "\u0002\u0002\u00e1\u00e2\u0003\u0002\u0002\u0002\u00e2\u00e4\u0003\u0002",
    "\u0002\u0002\u00e3\u00e1\u0003\u0002\u0002\u0002\u00e4\u00e5\u0007\u0004",
    "\u0002\u0002\u00e5\u0007\u0003\u0002\u0002\u0002\u00e6\u00e7\u0005\u0004",
    "\u0003\u0002\u00e7\t\u0003\u0002\u0002\u0002\u00e8\u00e9\u0005\u0004",
    "\u0003\u0002\u00e9\u000b\u0003\u0002\u0002\u0002\u00ea\u00eb\u0007\u001a",
    "\u0002\u0002\u00eb\r\u0003\u0002\u0002\u0002\u00ec\u00ed\u0007\u001a",
    "\u0002\u0002\u00ed\u000f\u0003\u0002\u0002\u0002\u00ee\u00ef\u0007\u001a",
    "\u0002\u0002\u00ef\u0011\u0003\u0002\u0002\u0002\u00f0\u00f1\u0007\u001a",
    "\u0002\u0002\u00f1\u0013\u0003\u0002\u0002\u0002\u00f2\u00f3\u0005\u0004",
    "\u0003\u0002\u00f3\u0015\u0003\u0002\u0002\u0002\u00f4\u00f5\u0005\u0004",
    "\u0003\u0002\u00f5\u0017\u0003\u0002\u0002\u0002\u00f6\u00f7\u0005\u0004",
    "\u0003\u0002\u00f7\u0019\u0003\u0002\u0002\u0002\u00f8\u00f9\u0005\u0004",
    "\u0003\u0002\u00f9\u001b\u0003\u0002\u0002\u0002\u00fa\u00fb\u0005\u0004",
    "\u0003\u0002\u00fb\u001d\u0003\u0002\u0002\u0002\u00fc\u00fd\u0007\u001a",
    "\u0002\u0002\u00fd\u00fe\u0005\u0004\u0003\u0002\u00fe\u001f\u0003\u0002",
    "\u0002\u0002\u00ff\u0100\u0005\u0004\u0003\u0002\u0100!\u0003\u0002",
    "\u0002\u0002\u0101\u0102\u0005\u0004\u0003\u0002\u0102#\u0003\u0002",
    "\u0002\u0002\u0018\'/8CJMS^mw~\u0081\u0087\u0090\u0099\u00a8\u00b2\u00bc",
    "\u00c4\u00ce\u00da\u00e1"].join("");


const atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

const decisionsToDFA = atn.decisionToState.map( (ds, index) => new antlr4.dfa.DFA(ds, index) );

const sharedContextCache = new antlr4.PredictionContextCache();

export default class VeLispParser extends antlr4.Parser {

    static grammarFileName = "VeLisp.g4";
    static literalNames = [ null, "'('", "')'", "' / '", "'.'", "'''" ];
    static symbolicNames = [ null, null, null, null, null, null, "AND", 
                             "COND", "DEFUN", "FOREACH", "FUNCTION", "IF", 
                             "LAMBDA", "OR", "PROGN", "QUOTE", "REPEAT", 
                             "SETQ", "WHILE", "NIL", "TRU", "INT", "REAL", 
                             "STR", "ID", "COMMENT", "LINE_COMMENT", "NEWLINE", 
                             "WHITESPACE" ];
    static ruleNames = [ "file", "expr", "condTestResult", "condTest", "condResult", 
                         "funName", "funParam", "funLocal", "foreachName", 
                         "foreachList", "ifTest", "ifThen", "ifElse", "repeatNum", 
                         "setqNameExpr", "whileTest", "listExpr" ];

    constructor(input) {
        super(input);
        this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
        this.ruleNames = VeLispParser.ruleNames;
        this.literalNames = VeLispParser.literalNames;
        this.symbolicNames = VeLispParser.symbolicNames;
    }

    get atn() {
        return atn;
    }



	file() {
	    let localctx = new FileContext(this, this._ctx, this.state);
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
	}



	expr() {
	    let localctx = new ExprContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 2, VeLispParser.RULE_expr);
	    var _la = 0; // Token type
	    try {
	        this.state = 216;
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
	            localctx = new FunctionContext(this, localctx);
	            this.enterOuterAlt(localctx, 5);
	            this.state = 97;
	            this.match(VeLispParser.T__0);
	            this.state = 98;
	            this.match(VeLispParser.FUNCTION);
	            this.state = 99;
	            this.expr();
	            this.state = 100;
	            this.match(VeLispParser.T__1);
	            break;

	        case 6:
	            localctx = new IfContext(this, localctx);
	            this.enterOuterAlt(localctx, 6);
	            this.state = 102;
	            this.match(VeLispParser.T__0);
	            this.state = 103;
	            this.match(VeLispParser.IF);
	            this.state = 104;
	            this.ifTest();
	            this.state = 105;
	            this.ifThen();
	            this.state = 107;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 106;
	                this.ifElse();
	            }

	            this.state = 109;
	            this.match(VeLispParser.T__1);
	            break;

	        case 7:
	            localctx = new LambdaContext(this, localctx);
	            this.enterOuterAlt(localctx, 7);
	            this.state = 111;
	            this.match(VeLispParser.T__0);
	            this.state = 112;
	            this.match(VeLispParser.LAMBDA);
	            this.state = 113;
	            this.match(VeLispParser.T__0);
	            this.state = 117;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeLispParser.ID) {
	                this.state = 114;
	                this.funParam();
	                this.state = 119;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 127;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            if(_la===VeLispParser.T__2) {
	                this.state = 120;
	                this.match(VeLispParser.T__2);
	                this.state = 124;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	                while(_la===VeLispParser.ID) {
	                    this.state = 121;
	                    this.funLocal();
	                    this.state = 126;
	                    this._errHandler.sync(this);
	                    _la = this._input.LA(1);
	                }
	            }

	            this.state = 129;
	            this.match(VeLispParser.T__1);
	            this.state = 131; 
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            do {
	                this.state = 130;
	                this.expr();
	                this.state = 133; 
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
	            this.state = 135;
	            this.match(VeLispParser.T__1);
	            break;

	        case 8:
	            localctx = new OrContext(this, localctx);
	            this.enterOuterAlt(localctx, 8);
	            this.state = 137;
	            this.match(VeLispParser.T__0);
	            this.state = 138;
	            this.match(VeLispParser.OR);
	            this.state = 142;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 139;
	                this.expr();
	                this.state = 144;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 145;
	            this.match(VeLispParser.T__1);
	            break;

	        case 9:
	            localctx = new PrognContext(this, localctx);
	            this.enterOuterAlt(localctx, 9);
	            this.state = 146;
	            this.match(VeLispParser.T__0);
	            this.state = 147;
	            this.match(VeLispParser.PROGN);
	            this.state = 151;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 148;
	                this.expr();
	                this.state = 153;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 154;
	            this.match(VeLispParser.T__1);
	            break;

	        case 10:
	            localctx = new QuoteContext(this, localctx);
	            this.enterOuterAlt(localctx, 10);
	            this.state = 155;
	            this.match(VeLispParser.T__0);
	            this.state = 156;
	            this.match(VeLispParser.QUOTE);
	            this.state = 157;
	            this.expr();
	            this.state = 158;
	            this.match(VeLispParser.T__1);
	            break;

	        case 11:
	            localctx = new RepeatContext(this, localctx);
	            this.enterOuterAlt(localctx, 11);
	            this.state = 160;
	            this.match(VeLispParser.T__0);
	            this.state = 161;
	            this.match(VeLispParser.REPEAT);
	            this.state = 162;
	            this.repeatNum();
	            this.state = 166;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 163;
	                this.expr();
	                this.state = 168;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 169;
	            this.match(VeLispParser.T__1);
	            break;

	        case 12:
	            localctx = new SetQContext(this, localctx);
	            this.enterOuterAlt(localctx, 12);
	            this.state = 171;
	            this.match(VeLispParser.T__0);
	            this.state = 172;
	            this.match(VeLispParser.SETQ);
	            this.state = 176;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while(_la===VeLispParser.ID) {
	                this.state = 173;
	                this.setqNameExpr();
	                this.state = 178;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 179;
	            this.match(VeLispParser.T__1);
	            break;

	        case 13:
	            localctx = new WhileContext(this, localctx);
	            this.enterOuterAlt(localctx, 13);
	            this.state = 180;
	            this.match(VeLispParser.T__0);
	            this.state = 181;
	            this.match(VeLispParser.WHILE);
	            this.state = 182;
	            this.whileTest();
	            this.state = 184; 
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            do {
	                this.state = 183;
	                this.expr();
	                this.state = 186; 
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
	            this.state = 188;
	            this.match(VeLispParser.T__1);
	            break;

	        case 14:
	            localctx = new DotListContext(this, localctx);
	            this.enterOuterAlt(localctx, 14);
	            this.state = 190;
	            this.match(VeLispParser.T__0);
	            this.state = 192; 
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            do {
	                this.state = 191;
	                this.listExpr();
	                this.state = 194; 
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            } while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0));
	            this.state = 196;
	            this.match(VeLispParser.T__3);
	            this.state = 197;
	            this.listExpr();
	            this.state = 198;
	            this.match(VeLispParser.T__1);
	            break;

	        case 15:
	            localctx = new ListContext(this, localctx);
	            this.enterOuterAlt(localctx, 15);
	            this.state = 200;
	            this.match(VeLispParser.T__0);
	            this.state = 204;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	            while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	                this.state = 201;
	                this.listExpr();
	                this.state = 206;
	                this._errHandler.sync(this);
	                _la = this._input.LA(1);
	            }
	            this.state = 207;
	            this.match(VeLispParser.T__1);
	            break;

	        case 16:
	            localctx = new NilContext(this, localctx);
	            this.enterOuterAlt(localctx, 16);
	            this.state = 208;
	            this.match(VeLispParser.NIL);
	            break;

	        case 17:
	            localctx = new TruContext(this, localctx);
	            this.enterOuterAlt(localctx, 17);
	            this.state = 209;
	            this.match(VeLispParser.TRU);
	            break;

	        case 18:
	            localctx = new IntContext(this, localctx);
	            this.enterOuterAlt(localctx, 18);
	            this.state = 210;
	            this.match(VeLispParser.INT);
	            break;

	        case 19:
	            localctx = new RealContext(this, localctx);
	            this.enterOuterAlt(localctx, 19);
	            this.state = 211;
	            this.match(VeLispParser.REAL);
	            break;

	        case 20:
	            localctx = new StrContext(this, localctx);
	            this.enterOuterAlt(localctx, 20);
	            this.state = 212;
	            this.match(VeLispParser.STR);
	            break;

	        case 21:
	            localctx = new IdContext(this, localctx);
	            this.enterOuterAlt(localctx, 21);
	            this.state = 213;
	            this.match(VeLispParser.ID);
	            break;

	        case 22:
	            localctx = new TickContext(this, localctx);
	            this.enterOuterAlt(localctx, 22);
	            this.state = 214;
	            this.match(VeLispParser.T__4);
	            this.state = 215;
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
	}



	condTestResult() {
	    let localctx = new CondTestResultContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 4, VeLispParser.RULE_condTestResult);
	    var _la = 0; // Token type
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 218;
	        this.match(VeLispParser.T__0);
	        this.state = 219;
	        this.condTest();
	        this.state = 223;
	        this._errHandler.sync(this);
	        _la = this._input.LA(1);
	        while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << VeLispParser.T__0) | (1 << VeLispParser.T__4) | (1 << VeLispParser.NIL) | (1 << VeLispParser.TRU) | (1 << VeLispParser.INT) | (1 << VeLispParser.REAL) | (1 << VeLispParser.STR) | (1 << VeLispParser.ID))) !== 0)) {
	            this.state = 220;
	            this.condResult();
	            this.state = 225;
	            this._errHandler.sync(this);
	            _la = this._input.LA(1);
	        }
	        this.state = 226;
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
	}



	condTest() {
	    let localctx = new CondTestContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 6, VeLispParser.RULE_condTest);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 228;
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
	}



	condResult() {
	    let localctx = new CondResultContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 8, VeLispParser.RULE_condResult);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 230;
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
	}



	funName() {
	    let localctx = new FunNameContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 10, VeLispParser.RULE_funName);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 232;
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
	}



	funParam() {
	    let localctx = new FunParamContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 12, VeLispParser.RULE_funParam);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 234;
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
	}



	funLocal() {
	    let localctx = new FunLocalContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 14, VeLispParser.RULE_funLocal);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 236;
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
	}



	foreachName() {
	    let localctx = new ForeachNameContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 16, VeLispParser.RULE_foreachName);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 238;
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
	}



	foreachList() {
	    let localctx = new ForeachListContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 18, VeLispParser.RULE_foreachList);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 240;
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
	}



	ifTest() {
	    let localctx = new IfTestContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 20, VeLispParser.RULE_ifTest);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 242;
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
	}



	ifThen() {
	    let localctx = new IfThenContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 22, VeLispParser.RULE_ifThen);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 244;
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
	}



	ifElse() {
	    let localctx = new IfElseContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 24, VeLispParser.RULE_ifElse);
	    try {
	        this.enterOuterAlt(localctx, 1);
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
	}



	repeatNum() {
	    let localctx = new RepeatNumContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 26, VeLispParser.RULE_repeatNum);
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
	}



	setqNameExpr() {
	    let localctx = new SetqNameExprContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 28, VeLispParser.RULE_setqNameExpr);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 250;
	        this.match(VeLispParser.ID);
	        this.state = 251;
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
	}



	whileTest() {
	    let localctx = new WhileTestContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 30, VeLispParser.RULE_whileTest);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 253;
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
	}



	listExpr() {
	    let localctx = new ListExprContext(this, this._ctx, this.state);
	    this.enterRule(localctx, 32, VeLispParser.RULE_listExpr);
	    try {
	        this.enterOuterAlt(localctx, 1);
	        this.state = 255;
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
	}


}

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
VeLispParser.FUNCTION = 10;
VeLispParser.IF = 11;
VeLispParser.LAMBDA = 12;
VeLispParser.OR = 13;
VeLispParser.PROGN = 14;
VeLispParser.QUOTE = 15;
VeLispParser.REPEAT = 16;
VeLispParser.SETQ = 17;
VeLispParser.WHILE = 18;
VeLispParser.NIL = 19;
VeLispParser.TRU = 20;
VeLispParser.INT = 21;
VeLispParser.REAL = 22;
VeLispParser.STR = 23;
VeLispParser.ID = 24;
VeLispParser.COMMENT = 25;
VeLispParser.LINE_COMMENT = 26;
VeLispParser.NEWLINE = 27;
VeLispParser.WHITESPACE = 28;

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

class FileContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_file;
    }

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFile(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class ExprContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_expr;
    }


	 
		copyFrom(ctx) {
			super.copyFrom(ctx);
		}

}


class OrContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	OR() {
	    return this.getToken(VeLispParser.OR, 0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitOr(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.OrContext = OrContext;

class PrognContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	PROGN() {
	    return this.getToken(VeLispParser.PROGN, 0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitProgn(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.PrognContext = PrognContext;

class RealContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	REAL() {
	    return this.getToken(VeLispParser.REAL, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitReal(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.RealContext = RealContext;

class TickContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitTick(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.TickContext = TickContext;

class CondContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	COND() {
	    return this.getToken(VeLispParser.COND, 0);
	};

	condTestResult = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(CondTestResultContext);
	    } else {
	        return this.getTypedRuleContext(CondTestResultContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitCond(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.CondContext = CondContext;

class WhileContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	WHILE() {
	    return this.getToken(VeLispParser.WHILE, 0);
	};

	whileTest() {
	    return this.getTypedRuleContext(WhileTestContext,0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitWhile(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.WhileContext = WhileContext;

class ListContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	listExpr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ListExprContext);
	    } else {
	        return this.getTypedRuleContext(ListExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitList(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.ListContext = ListContext;

class DefunContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	DEFUN() {
	    return this.getToken(VeLispParser.DEFUN, 0);
	};

	funName() {
	    return this.getTypedRuleContext(FunNameContext,0);
	};

	funParam = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(FunParamContext);
	    } else {
	        return this.getTypedRuleContext(FunParamContext,i);
	    }
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	funLocal = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(FunLocalContext);
	    } else {
	        return this.getTypedRuleContext(FunLocalContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitDefun(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.DefunContext = DefunContext;

class IntContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	INT() {
	    return this.getToken(VeLispParser.INT, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitInt(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.IntContext = IntContext;

class NilContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	NIL() {
	    return this.getToken(VeLispParser.NIL, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitNil(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.NilContext = NilContext;

class StrContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	STR() {
	    return this.getToken(VeLispParser.STR, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitStr(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.StrContext = StrContext;

class ForeachContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	FOREACH() {
	    return this.getToken(VeLispParser.FOREACH, 0);
	};

	foreachName() {
	    return this.getTypedRuleContext(ForeachNameContext,0);
	};

	foreachList() {
	    return this.getTypedRuleContext(ForeachListContext,0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitForeach(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.ForeachContext = ForeachContext;

class LambdaContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	LAMBDA() {
	    return this.getToken(VeLispParser.LAMBDA, 0);
	};

	funParam = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(FunParamContext);
	    } else {
	        return this.getTypedRuleContext(FunParamContext,i);
	    }
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	funLocal = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(FunLocalContext);
	    } else {
	        return this.getTypedRuleContext(FunLocalContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitLambda(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.LambdaContext = LambdaContext;

class QuoteContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	QUOTE() {
	    return this.getToken(VeLispParser.QUOTE, 0);
	};

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitQuote(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.QuoteContext = QuoteContext;

class AndContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	AND() {
	    return this.getToken(VeLispParser.AND, 0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitAnd(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.AndContext = AndContext;

class TruContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	TRU() {
	    return this.getToken(VeLispParser.TRU, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitTru(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.TruContext = TruContext;

class FunctionContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	FUNCTION() {
	    return this.getToken(VeLispParser.FUNCTION, 0);
	};

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFunction(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.FunctionContext = FunctionContext;

class RepeatContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	REPEAT() {
	    return this.getToken(VeLispParser.REPEAT, 0);
	};

	repeatNum() {
	    return this.getTypedRuleContext(RepeatNumContext,0);
	};

	expr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ExprContext);
	    } else {
	        return this.getTypedRuleContext(ExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitRepeat(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.RepeatContext = RepeatContext;

class DotListContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	listExpr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(ListExprContext);
	    } else {
	        return this.getTypedRuleContext(ListExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitDotList(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.DotListContext = DotListContext;

class SetQContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	SETQ() {
	    return this.getToken(VeLispParser.SETQ, 0);
	};

	setqNameExpr = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(SetqNameExprContext);
	    } else {
	        return this.getTypedRuleContext(SetqNameExprContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitSetQ(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.SetQContext = SetQContext;

class IdContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitId(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.IdContext = IdContext;

class IfContext extends ExprContext {

    constructor(parser, ctx) {
        super(parser);
        super.copyFrom(ctx);
    }

	IF() {
	    return this.getToken(VeLispParser.IF, 0);
	};

	ifTest() {
	    return this.getTypedRuleContext(IfTestContext,0);
	};

	ifThen() {
	    return this.getTypedRuleContext(IfThenContext,0);
	};

	ifElse() {
	    return this.getTypedRuleContext(IfElseContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitIf(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}

VeLispParser.IfContext = IfContext;

class CondTestResultContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_condTestResult;
    }

	condTest() {
	    return this.getTypedRuleContext(CondTestContext,0);
	};

	condResult = function(i) {
	    if(i===undefined) {
	        i = null;
	    }
	    if(i===null) {
	        return this.getTypedRuleContexts(CondResultContext);
	    } else {
	        return this.getTypedRuleContext(CondResultContext,i);
	    }
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitCondTestResult(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class CondTestContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_condTest;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitCondTest(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class CondResultContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_condResult;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitCondResult(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class FunNameContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_funName;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFunName(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class FunParamContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_funParam;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFunParam(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class FunLocalContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_funLocal;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitFunLocal(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class ForeachNameContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_foreachName;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitForeachName(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class ForeachListContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_foreachList;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitForeachList(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class IfTestContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_ifTest;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitIfTest(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class IfThenContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_ifThen;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitIfThen(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class IfElseContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_ifElse;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitIfElse(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class RepeatNumContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_repeatNum;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitRepeatNum(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class SetqNameExprContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_setqNameExpr;
    }

	ID() {
	    return this.getToken(VeLispParser.ID, 0);
	};

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitSetqNameExpr(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class WhileTestContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_whileTest;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitWhileTest(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}



class ListExprContext extends antlr4.ParserRuleContext {

    constructor(parser, parent, invokingState) {
        if(parent===undefined) {
            parent = null;
        }
        if(invokingState===undefined || invokingState===null) {
            invokingState = -1;
        }
        super(parent, invokingState);
        this.parser = parser;
        this.ruleIndex = VeLispParser.RULE_listExpr;
    }

	expr() {
	    return this.getTypedRuleContext(ExprContext,0);
	};

	accept(visitor) {
	    if ( visitor instanceof VeLispVisitor ) {
	        return visitor.visitListExpr(this);
	    } else {
	        return visitor.visitChildren(this);
	    }
	}


}




VeLispParser.FileContext = FileContext; 
VeLispParser.ExprContext = ExprContext; 
VeLispParser.CondTestResultContext = CondTestResultContext; 
VeLispParser.CondTestContext = CondTestContext; 
VeLispParser.CondResultContext = CondResultContext; 
VeLispParser.FunNameContext = FunNameContext; 
VeLispParser.FunParamContext = FunParamContext; 
VeLispParser.FunLocalContext = FunLocalContext; 
VeLispParser.ForeachNameContext = ForeachNameContext; 
VeLispParser.ForeachListContext = ForeachListContext; 
VeLispParser.IfTestContext = IfTestContext; 
VeLispParser.IfThenContext = IfThenContext; 
VeLispParser.IfElseContext = IfElseContext; 
VeLispParser.RepeatNumContext = RepeatNumContext; 
VeLispParser.SetqNameExprContext = SetqNameExprContext; 
VeLispParser.WhileTestContext = WhileTestContext; 
VeLispParser.ListExprContext = ListExprContext; 
