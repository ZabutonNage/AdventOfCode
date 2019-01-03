const crypto = require(`crypto`);

const input = `bgvyzdsv`;


const result = findSecretNumber(1);
console.log(result);


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
