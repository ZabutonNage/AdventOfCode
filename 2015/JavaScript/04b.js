const inputFile = `../_input/04.input`;
const input = getInput(inputFile);

const crypto = require(`crypto`);


console.log(
    findSecretNumber(1)
);


function findSecretNumber(current) {
    const hash = hashWithInput(current);
    return hash.startsWith(`000000`)
        ? current
        : findSecretNumber(current +1);
}

function hashWithInput(num) {
    const hash = crypto.createHash(`md5`);
    hash.update(input);
    hash.update(num.toString());
    return hash.digest(`hex`);
}


function getInput(inputFile) {
    const { join } = require(`path`);
    const { readFileSync } = require(`fs`);

    return readFileSync(join(__dirname, inputFile), `utf8`);
}
