console.log(solve(getInput().split(`\n`)));


function solve(words) {
    const charSums = getCharSums(words);
    const match = findMatch(charSums);
    return match && intersectStrict(match);
}


function findMatch([current, ...rest]) {
    if (!current) return undefined;

    const [sum, word] = current;
    const candidates = rest.filter(sw => sw[0] <= sum + 25).map(sw => sw[1]);
    const match = getMatch(word, candidates);

    return match || findMatch(rest);
}

function getMatch(a, bs) {
    if (!bs.length) return undefined;

    const splitA = a.split(``);
    const splitB = bs[0].split(``);

    const diff = splitB.map((c, i) => splitA[i].charCodeAt(0) !== c.charCodeAt(0)).filter(x => x).length;
    return diff === 1
        ? [a, bs[0]]
        : getMatch(a, bs.slice(1));
}

function getCharSums(words) {
    return words.map(w => ([charSum(w), w])).sort((a, b) => a[0] - b[0]);

    function charSum(word) {
        return word.split(``).reduce((acc, c) => acc + c.charCodeAt(0), 0);
    }
}

function intersectStrict([a, b]) {
    const as = a.split(``);
    const bs = b.split(``);
    return as.reduce((acc, c, i) => c === bs[i] ? acc + c : acc, ``);
}


function getInput() {
    // return `abc\nabd`;
    // return `abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz`;
    return `evsialkqydurohxqpwbcugtjmh
evsialkqydurohxzssbcngtjmv
fvlialkqydurohxzpwbcngujmf
nvsialkqydorohxzpwpcngtjmf
evsialjqydnrohxypwbcngtjmf
vvsialyqxdurohxzpwbcngtjmf
yvsialksydurowxzpwbcngtjmf
evsillkqydurbhxzpmbcngtjmf
ivsialkqyxurshxzpwbcngtjmf
ejsiagkqyduhohxzpwbcngtjmf
evsialkqldurohxzpcbcngtjmi
evsialkqydurohxzpsbyngtkmf
ersialkeydurohxzpwbcngtpmf
evsialuqzdkrohxzpwbcngtjmf
evswulkpydurohxzpwbcngtjmf
evsialkqyiurohxzpwucngttmf
evtialkqydurphxzywbcngtjmf
evsialkzyiurohxzpwbcxgtjmf
evsiaykqydurohxzpwbcggtjuf
evxqalkqydurohmzpwbcngtjmf
eisralkqydurohxzpdbcngtjmf
evsfalkqydurohxzpwbangtjwf
evbialkqydurohxzawbcngtjmg
evsialkqydrrohxrpcbcngtjmf
evsialkqycurohxzpvbcngtjkf
evsialkqsdudohxzpwbcnotjmf
evsiackqydurohxzpmbsngtjmf
evsialmqykurohxzpwbfngtjmf
evsialsqydurohxzpwucngtjxf
tvsialkqyeurohxzpwbcrgtjmf
zvsialkqydbrohxzpwbcnltjmf
evsmbskqydurohxzpwbcngtjmf
evsialkqydurohxzpwbcngpgmt
evsialkqydurlyezpwbcngtjmf
evoialkqyturohxzpwbcnjtjmf
evsialkqydurohxspkfcngtjmf
evsiaikqydurohxjpwbcngtjmd
evsialkyydurohxzvwbcngtjmc
svsialkqyduhohxzpwbhngtjmf
eysillkqydurohxzhwbcngtjmf
evsialkqyduetaxzpwbcngtjmf
evsialkqxdurshxzpwbcngtjmb
evsiadkqydwrovxzpwbcngtjmf
evsialkqydurokxzpwbcngjjef
evskalkqymurohxzpybcngtjmf
cvsialkqydurohxzpwbcnbtjma
evsialkqydurohxzawhcngtjuf
evsiahkqfduroixzpwbcngtjmf
evsivlkqyduroqxzpwbctgtjmf
evsiarkqyduroixzywbcngtjmf
evspalkqydurohxzpwlcngxjmf
eesialkqydurohxzpalcngtjmf
gvsualkqydurohxzpwbmngtjmf
evsialkqydurlhxzpwbcngsjmq
evsialhqydfrohxopwbcngtjmf
evzialkqydsrohxzpwbcngtjmw
evbpalkqydurbhxzpwbcngtjmf
mvsialkqydurohxzpwbcnghjmr
evsialkqsdurohxzpkbcngtjxf
ejkialktydurohxzpwbcngtjmf
evsialkqyauoohxzpwbqngtjmf
evsiklkyyduroqxzpwbcngtjmf
evgialkqydurohxzpwocngthmf
ebsialkqydcrohxzpwbcngtbmf
evsialkqysurohxzpwfingtjmf
evsialkqddurmhxzpwbnngtjmf
evsialkqydurohxoiwwcngtjmf
evsialkqydurohpzkzbcngtjmf
vvsealkqydurorxzpwbcngtjmf
evsialkqyduroqxzpwlungtjmf
eviialkqiyurohxzpwbcngtjmf
evzsalkqyaurohxzpwbcngtjmf
exsialkqydurohfzpwbwngtjmf
evsialkqyduruhxkpwbcnytjmf
essiatkqydurohxzpwbxngtjmf
evsialkqyduroamzpwbcngtjcf
wvsialkqyduruhxzpwbcnxtjmf
evsialkqydurohxgpwbcngtjeh
evsialfqxdurohxzpwbcngtomf
evsialkqyourghxzpwbcngtbmf
evsoaokqydurohxzpwbcngtamf
evsialpqydurohxzpwccxgtjmf
evsialkqzdurxhxgpwbcngtjmf
ezsialkqmdurohxzpwbcngtjmi
cvsialjeydurohxzpwbcngtjmf
evsialkqydurocxupwbcvgtjmf
evscalkqydtrohxzpebcngtjmf
evjialkqyduiohxzpabcngtjmf
evsialjqyduruhxzppbcngtjmf
evsialkqydurfhxzpwbcuqtjmf
evsialkqyiurohizpwucngttmf
evsialiqydurrhxzpwbcngdjmf
evbialkqywurohxzpwhcngtjmf
evsialkqyduloyxzpwbqngtjmf
evsialxqyduzohxzpwbqngtjmf
vvsialkqydurohxzpwbcnqpjmf
evsialksydurohxzcwbmngtjmf
pvsialkqydurohxzpwucngtjvf
evsialkqydurohmkpwbcngtfmf
mvsialkqydurphyzpwbcngtjmf
evsialkqydyrohxzhwbcnitjmf
evsialokydurozxzpwbcngtjmf
evsialkqyduroexfcwbcngtjmf
evsiavkqydurohxzpwbcnmtjme
evsiawkqydurohxzpwbcngojjf
evsialkaydurohxzpwfcngtjff
evsialkaydurohxzpwbcngtjpb
gvsialkqyburorxzpwbcngtjmf
evszalkqydurphxzpwocngtjmf
evsualkqyduropxzpwbcngejmf
evsitlkqydurshxzpwbcngtkmf
evbixlkqydrrohxzpwbcngtjmf
elsialkqydprohxzpwbcngtrmf
evsialkqydurohbzpwbcggtjmc
evtoalqqydurohxzpwbcngtjmf
evsralhqydurohxzowbcngtjmf
evsialkhydurohxzlsbcngtjmf
evsialkqydurohxvpwbcnuujmf
evsialkqydurocxzuwbcngtjmi
evsialkqndyrokxzpwbcngtjmf
evsialkqydurywfzpwbcngtjmf
evsialkqydurohxzwwbcngthms
eqsiahkqydurohxzpwbyngtjmf
evsdalkqydurohxzpwbcnjkjmf
evsialkqyddrohplpwbcngtjmf
evshalkqydurohxzpfxcngtjmf
evvialkqydurohxapwbcngtjmh
evsialkqyduvohxzpwbcnnvjmf
evsiblkqedurohxzpwbkngtjmf
evsvalkqfdutohxzpwbcngtjmf
evsialjqydurohxzpwbcnctjsf
evsialkxywurohxdpwbcngtjmf
evsiagkqydurohxzpwzcjgtjmf
ebsialkqydurohxzpxfcngtjmf
evsialkqysfrohxzpwbcngtjlf
evvialkqyqurwhxzpwbcngtjmf
evxialkqydurohxzpwgcnrtjmf
vvsillkqydurohxzpwbcvgtjmf
evsiwlkqyduoohxzpwbcngtjxf
evsialkqypurohezpwbcngtjwf
evbialkqydurohxipwbcnftjmf
evsiakkqyduyohxzpwbcngtjmu
evsialkqydurohzzpwxqngtjmf
evsialkqykurkhxzpwocngtjmf
dvriplkqydurohxzpwbcngtjmf
evsialkqgdurohxzpwbmnctjmf
evsialkqyuurohxzpwtcngtjmj
wvsialkqydurohxzpwbchgejmf
eusimlsqydurohxzpwbcngtjmf
evsialkqydqrohxzhwbcngtjmh
wvswalkqydurohxzpwbcngjjmf
evsialkqyourohxzkwbcngttmf
evaialkqydurohxzbubcngtjmf
evfialkqydueohxzpwbclgtjmf
evrialkqydurohxzpwbcnctjmh
evsiaojqydxrohxzpwbcngtjmf
evsualkqywuxohxzpwbcngtjmf
evsialkdydrzohxzpwbcngtjmf
evlialkqyfurohxzpwbcnotjmf
epsialkqydujohxzpwbcngtjif
evsialkqyaucohxgpwbcngtjmf
lvsialaqydurohxzpwbcngtjzf
evsialkgydurohezpwbcngtjmo
lvsialkqydurosxwpwbcngtjmf
evsiaekqyqurohxzpvbcngtjmf
evsiapkqydirohxzpwbzngtjmf
zvsixlkwydurohxzpwbcngtjmf
evaialkqyduoohxzpwbcngtjkf
evsialcqedurohxzpwbcngtjmc
evjialkgydurohxzpwbwngtjmf
evsialkqcdurohxzpwbcpgojmf
evsialkqkdurohxzlwbcngtrmf
eosiylkzydurohxzpwbcngtjmf
evsialkqydurohhzpwscnmtjmf
evsiallqydurobxzpwbxngtjmf
evsialkqydurohwztwhcngtjmf
evsiallqydurohxzpwbcygjjmf
evsiabkqywurohxzpwbcngtjmy
evsiackqydzrohxznwbcngtjmf
evsiazkqzdurooxzpwbcngtjmf
evsialcqydurghxzpwbcngtjmc
yvsiaxkqydurohxzpwbcxgtjmf
evsiylkqgdhrohxzpwbcngtjmf
lvsialkqydurohxgcwbcngtjmf
evsiglkqydurohxzpwbvngzjmf
evsialkqyvurohxzpwbcngtjnz
evsialkgydueohxzpwbcpgtjmf
cvsiavkqyddrohxzpwbcngtjmf
evsialklyrurohxzpwbcngtjff
eisialkqyduwohxzpwbcngcjmf
evsialkqydrrihwzpwbcngtjmf
easialkqydurohxzpwbcnltrmf
evsialfqydurohxzpybcnytjmf
eqsialkqycurohxzywbcngtjmf
evsitlkqmdurohxzpwbcngtjmx
evsiclsqyduroixzpwbcngtjmf
elsialrqydurohxzpwmcngtjmf
evsiapkqodurohxzpwbcogtjmf
evstalkeydurohxzpibcngtjmf
evsihlkqyqurohxzpwblngtjmf
euszalkqydurohxipwbcngtjmf
ezsialksydurohxzpwbcngfjmf
eisialkdydurohxzpwbcngtumf
evsirlkaydprohxzpwbcngtjmf
evsiklkqydnrohxzpwbcngtjmu
evsialkqydnuohxzpwbcngtjmu
eksialkqydurohxztwfcngtjmf
evlialkqedurohxzpwbhngtjmf
evqialkqydurohxzpubcngtjpf
evsialkwydurohwzpwbcnmtjmf
evsiaokqcdurohxzpwbcngtjcf
evsialkkyfurohxzpvbcngtjmf
evsialkqyduromxzpwqcngtimf
evsialkqydumohxzpwbcnmtjsf
evsialddydurehxzpwbcngtjmf
evsialkqydurohxzpobcnptjmk
evsiagkqydurohhzpwbcxgtjmf
evsfalkqydurohszpwbangtjmf
evgialkzyduqohxzpwbcngtjmf
evaialkqzdurohxzpwbcngtjmo
evsialkqyqurohxjpwbcnntjmf
evsialkjydybohxzpwbcngtjmf
evskalgqydurohxzrwbcngtjmf
evsialkqydurohxzpjbcymtjmf
evsialkqqdurohxzpybcngtjyf
evsialkqydqrbhxzpwbcngtjmj
evssalaqrdurohxzpwbcngtjmf
mvsialkfydurohxzpwbcngtjmk
evsialkqwdurohxzpwgcngtjdf
evqkalkqydurohxzpwbcngajmf
evbialkqydurohxzpibcngejmf
evszalkqydurbhxzpwbcngtjsf
evsialkqydurohxepwbcngtjjo
evsialkqcdubmhxzpwbcngtjmf
evsiarkqyduroaxzpwbcngtjmp
evsiakkqyduzohczpwbcngtjmf
evtualkqydurofxzpwbcngtjmf
ejsialkqvdurohzzpwbcngtjmf
evsialkqydurohczpwbcngqvmf
svsianfqydurohxzpwbcngtjmf
evsialiqydurohxzpwbcngzqmf
ejsialhqydurohxzpwjcngtjmf
evpialkqydurohxzpwbcnbtjff
evsialkuyvurohxzpwbcngtjkf
eqsialkqydurohxzpwbcnwtcmf
evsiatkqydkrohxzpwkcngtjmf
evsialkqydurohxzpebciytjmf
evsialkqydrrohxzpwtcngtfmf
evsialkqjducohxzpwycngtjmf
evsialkqydurohxzpwicnxtjnf`;
}
