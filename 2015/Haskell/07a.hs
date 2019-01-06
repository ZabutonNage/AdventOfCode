import Data.Bits
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (isInfixOf, isPrefixOf, sortBy)
import Data.Word (Word16)
import Data.Map.Strict (Map, empty, insert, (!))


main = print . solve $ lines input


solve :: [String] -> Maybe Word16
solve inp = fmap (resolve resolved . parseSource) $ lookup "a" wires
    where
    resolved = resolveSources empty $ tail wires
    wires = sortBy sortWires $ toKV <$> inp
    sortWires ([], _) ([], _) = EQ
    sortWires (a:_, _) ([], _) = GT
    sortWires ([], _) (b:_, _) = LT
    sortWires (a:as, _) (b:bs, _)
        | a == b = sortWires (as, []) (bs, [])
        | length as /= length bs = compare (length as) (length bs)
        | otherwise = compare a b


type ResolvedMap = Map String Word16

resolveSources :: ResolvedMap -> [(String, String)] -> ResolvedMap
resolveSources resolved [] = resolved
resolveSources resolved ((key, val):wires) = resolveSources (nextMap $ parseSource val) wires
    where
    nextMap (Const v) = insert key v                      resolved
    nextMap src       = insert key (resolve resolved src) resolved

resolve :: ResolvedMap -> Source -> Word16
resolve _ (Const val) = val
resolve m (Wire w) = m ! w
resolve m (Gate (Not s)) = complement $ resolve m s
resolve m (Gate (And s1 s2)) = (resolve m s1) .&. (resolve m s2)
resolve m (Gate (Or s1 s2)) = (resolve m s1) .|. (resolve m s2)
resolve m (Gate (ShiftL s n)) = shiftL (resolve m s) n
resolve m (Gate (ShiftR s n)) = shiftR (resolve m s) n

parseSource :: String -> Source
parseSource str
    | all isDigit str = Const $ read str
    | all isAlpha str = Wire str
    | "NOT" `isPrefixOf` str = Gate . Not . parseSource $ drop 4 str
    | "AND" `isInfixOf` str = Gate $ And (parseSource $ leftSrc str) (parseSource $ rightSrc str)
    | "OR" `isInfixOf` str = Gate $ Or (parseSource $ leftSrc str) (parseSource $ rightSrc str)
    | "LSHIFT" `isInfixOf` str = Gate $ ShiftL (parseSource $ leftSrc str) (read $ rightSrc str)
    | "RSHIFT" `isInfixOf` str = Gate $ ShiftR (parseSource $ leftSrc str) (read $ rightSrc str)
    where
    leftSrc = takeWhile isAlphaNum
    rightSrc = reverse . takeWhile isAlphaNum . reverse

toKV :: String -> (String, String)
toKV str = (key, val)
    where
    key = drop 2 $ dropWhile (/= '>') str
    val = init $ takeWhile (/= '-') str


data Source
    = Const Word16
    | Wire String
    | Gate Gate

data Gate
    = Not Source
    | And Source Source
    | Or Source Source
    | ShiftL Source Int
    | ShiftR Source Int


input = "lf AND lq -> ls\niu RSHIFT 1 -> jn\nbo OR bu -> bv\ngj RSHIFT 1 -> hc\net RSHIFT 2 -> eu\nbv AND bx -> by\nis OR it -> iu\nb OR n -> o\ngf OR ge -> gg\nNOT kt -> ku\nea AND eb -> ed\nkl OR kr -> ks\nhi AND hk -> hl\nau AND av -> ax\nlf RSHIFT 2 -> lg\ndd RSHIFT 3 -> df\neu AND fa -> fc\ndf AND dg -> di\nip LSHIFT 15 -> it\nNOT el -> em\net OR fe -> ff\nfj LSHIFT 15 -> fn\nt OR s -> u\nly OR lz -> ma\nko AND kq -> kr\nNOT fx -> fy\net RSHIFT 1 -> fm\neu OR fa -> fb\ndd RSHIFT 2 -> de\nNOT go -> gp\nkb AND kd -> ke\nhg OR hh -> hi\njm LSHIFT 1 -> kg\nNOT cn -> co\njp RSHIFT 2 -> jq\njp RSHIFT 5 -> js\n1 AND io -> ip\neo LSHIFT 15 -> es\n1 AND jj -> jk\ng AND i -> j\nci RSHIFT 3 -> ck\ngn AND gp -> gq\nfs AND fu -> fv\nlj AND ll -> lm\njk LSHIFT 15 -> jo\niu RSHIFT 3 -> iw\nNOT ii -> ij\n1 AND cc -> cd\nbn RSHIFT 3 -> bp\nNOT gw -> gx\nNOT ft -> fu\njn OR jo -> jp\niv OR jb -> jc\nhv OR hu -> hw\n19138 -> b\ngj RSHIFT 5 -> gm\nhq AND hs -> ht\ndy RSHIFT 1 -> er\nao OR an -> ap\nld OR le -> lf\nbk LSHIFT 1 -> ce\nbz AND cb -> cc\nbi LSHIFT 15 -> bm\nil AND in -> io\naf AND ah -> ai\nas RSHIFT 1 -> bl\nlf RSHIFT 3 -> lh\ner OR es -> et\nNOT ax -> ay\nci RSHIFT 1 -> db\net AND fe -> fg\nlg OR lm -> ln\nk AND m -> n\nhz RSHIFT 2 -> ia\nkh LSHIFT 1 -> lb\nNOT ey -> ez\nNOT di -> dj\ndz OR ef -> eg\nlx -> a\nNOT iz -> ja\ngz LSHIFT 15 -> hd\nce OR cd -> cf\nfq AND fr -> ft\nat AND az -> bb\nha OR gz -> hb\nfp AND fv -> fx\nNOT gb -> gc\nia AND ig -> ii\ngl OR gm -> gn\n0 -> c\nNOT ca -> cb\nbn RSHIFT 1 -> cg\nc LSHIFT 1 -> t\niw OR ix -> iy\nkg OR kf -> kh\ndy OR ej -> ek\nkm AND kn -> kp\nNOT fc -> fd\nhz RSHIFT 3 -> ib\nNOT dq -> dr\nNOT fg -> fh\ndy RSHIFT 2 -> dz\nkk RSHIFT 2 -> kl\n1 AND fi -> fj\nNOT hr -> hs\njp RSHIFT 1 -> ki\nbl OR bm -> bn\n1 AND gy -> gz\ngr AND gt -> gu\ndb OR dc -> dd\nde OR dk -> dl\nas RSHIFT 5 -> av\nlf RSHIFT 5 -> li\nhm AND ho -> hp\ncg OR ch -> ci\ngj AND gu -> gw\nge LSHIFT 15 -> gi\ne OR f -> g\nfp OR fv -> fw\nfb AND fd -> fe\ncd LSHIFT 15 -> ch\nb RSHIFT 1 -> v\nat OR az -> ba\nbn RSHIFT 2 -> bo\nlh AND li -> lk\ndl AND dn -> do\neg AND ei -> ej\nex AND ez -> fa\nNOT kp -> kq\nNOT lk -> ll\nx AND ai -> ak\njp OR ka -> kb\nNOT jd -> je\niy AND ja -> jb\njp RSHIFT 3 -> jr\nfo OR fz -> ga\ndf OR dg -> dh\ngj RSHIFT 2 -> gk\ngj OR gu -> gv\nNOT jh -> ji\nap LSHIFT 1 -> bj\nNOT ls -> lt\nir LSHIFT 1 -> jl\nbn AND by -> ca\nlv LSHIFT 15 -> lz\nba AND bc -> bd\ncy LSHIFT 15 -> dc\nln AND lp -> lq\nx RSHIFT 1 -> aq\ngk OR gq -> gr\nNOT kx -> ky\njg AND ji -> jj\nbn OR by -> bz\nfl LSHIFT 1 -> gf\nbp OR bq -> br\nhe OR hp -> hq\net RSHIFT 5 -> ew\niu RSHIFT 2 -> iv\ngl AND gm -> go\nx OR ai -> aj\nhc OR hd -> he\nlg AND lm -> lo\nlh OR li -> lj\nda LSHIFT 1 -> du\nfo RSHIFT 2 -> fp\ngk AND gq -> gs\nbj OR bi -> bk\nlf OR lq -> lr\ncj AND cp -> cr\nhu LSHIFT 15 -> hy\n1 AND bh -> bi\nfo RSHIFT 3 -> fq\nNOT lo -> lp\nhw LSHIFT 1 -> iq\ndd RSHIFT 1 -> dw\ndt LSHIFT 15 -> dx\ndy AND ej -> el\nan LSHIFT 15 -> ar\naq OR ar -> as\n1 AND r -> s\nfw AND fy -> fz\nNOT im -> in\net RSHIFT 3 -> ev\n1 AND ds -> dt\nec AND ee -> ef\nNOT ak -> al\njl OR jk -> jm\n1 AND en -> eo\nlb OR la -> lc\niu AND jf -> jh\niu RSHIFT 5 -> ix\nbo AND bu -> bw\ncz OR cy -> da\niv AND jb -> jd\niw AND ix -> iz\nlf RSHIFT 1 -> ly\niu OR jf -> jg\nNOT dm -> dn\nlw OR lv -> lx\ngg LSHIFT 1 -> ha\nlr AND lt -> lu\nfm OR fn -> fo\nhe RSHIFT 3 -> hg\naj AND al -> am\n1 AND kz -> la\ndy RSHIFT 5 -> eb\njc AND je -> jf\ncm AND co -> cp\ngv AND gx -> gy\nev OR ew -> ex\njp AND ka -> kc\nfk OR fj -> fl\ndy RSHIFT 3 -> ea\nNOT bs -> bt\nNOT ag -> ah\ndz AND ef -> eh\ncf LSHIFT 1 -> cz\nNOT cv -> cw\n1 AND cx -> cy\nde AND dk -> dm\nck AND cl -> cn\nx RSHIFT 5 -> aa\ndv LSHIFT 1 -> ep\nhe RSHIFT 2 -> hf\nNOT bw -> bx\nck OR cl -> cm\nbp AND bq -> bs\nas OR bd -> be\nhe AND hp -> hr\nev AND ew -> ey\n1 AND lu -> lv\nkk RSHIFT 3 -> km\nb AND n -> p\nNOT kc -> kd\nlc LSHIFT 1 -> lw\nkm OR kn -> ko\nid AND if -> ig\nih AND ij -> ik\njr AND js -> ju\nci RSHIFT 5 -> cl\nhz RSHIFT 1 -> is\n1 AND ke -> kf\nNOT gs -> gt\naw AND ay -> az\nx RSHIFT 2 -> y\nab AND ad -> ae\nff AND fh -> fi\nci AND ct -> cv\neq LSHIFT 1 -> fk\ngj RSHIFT 3 -> gl\nu LSHIFT 1 -> ao\nNOT bb -> bc\nNOT hj -> hk\nkw AND ky -> kz\nas AND bd -> bf\ndw OR dx -> dy\nbr AND bt -> bu\nkk AND kv -> kx\nep OR eo -> eq\nhe RSHIFT 1 -> hx\nki OR kj -> kk\nNOT ju -> jv\nek AND em -> en\nkk RSHIFT 5 -> kn\nNOT eh -> ei\nhx OR hy -> hz\nea OR eb -> ec\ns LSHIFT 15 -> w\nfo RSHIFT 1 -> gh\nkk OR kv -> kw\nbn RSHIFT 5 -> bq\nNOT ed -> ee\n1 AND ht -> hu\ncu AND cw -> cx\nb RSHIFT 5 -> f\nkl AND kr -> kt\niq OR ip -> ir\nci RSHIFT 2 -> cj\ncj OR cp -> cq\no AND q -> r\ndd RSHIFT 5 -> dg\nb RSHIFT 2 -> d\nks AND ku -> kv\nb RSHIFT 3 -> e\nd OR j -> k\nNOT p -> q\nNOT cr -> cs\ndu OR dt -> dv\nkf LSHIFT 15 -> kj\nNOT ac -> ad\nfo RSHIFT 5 -> fr\nhz OR ik -> il\njx AND jz -> ka\ngh OR gi -> gj\nkk RSHIFT 1 -> ld\nhz RSHIFT 5 -> ic\nas RSHIFT 2 -> at\nNOT jy -> jz\n1 AND am -> an\nci OR ct -> cu\nhg AND hh -> hj\njq OR jw -> jx\nv OR w -> x\nla LSHIFT 15 -> le\ndh AND dj -> dk\ndp AND dr -> ds\njq AND jw -> jy\nau OR av -> aw\nNOT bf -> bg\nz OR aa -> ab\nga AND gc -> gd\nhz AND ik -> im\njt AND jv -> jw\nz AND aa -> ac\njr OR js -> jt\nhb LSHIFT 1 -> hv\nhf OR hl -> hm\nib OR ic -> id\nfq OR fr -> fs\ncq AND cs -> ct\nia OR ig -> ih\ndd OR do -> dp\nd AND j -> l\nib AND ic -> ie\nas RSHIFT 3 -> au\nbe AND bg -> bh\ndd AND do -> dq\nNOT l -> m\n1 AND gd -> ge\ny AND ae -> ag\nfo AND fz -> gb\nNOT ie -> if\ne AND f -> h\nx RSHIFT 3 -> z\ny OR ae -> af\nhf AND hl -> hn\nNOT h -> i\nNOT hn -> ho\nhe RSHIFT 5 -> hh"
