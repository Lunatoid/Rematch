
function executeRegexAndFormat(regex, inputs) {
    if (!(regex instanceof RegExp)) {
        throw new Error("The first argument must be a valid regular expression.");
    }
    if (!Array.isArray(inputs)) {
        throw new Error("The second argument must be an array of input strings.");
    }

    const formattedResult = {
        pattern: regex.source,
        inputs: inputs.map(input => {
            const matches = [];
            let match;
            while ((match = regex.exec(input)) !== null) {
                matches.push(Array.from(match));
                if (!regex.global) break;
            }

            return {
                text: input,
                matches: matches
            };
        })
    };

    const output = JSON.stringify(formattedResult, null, 2);
    console.log(output);

    return formattedResult;
}

const args = process.argv.slice(2);
if (args.length < 2) {
    console.error("Usage: node create-test.js <regex> <input1> [<input2> ...]");
    process.exit(1);
}

const [regexString, ...inputs] = args;

let regex;
try {
    regex = new RegExp(regexString, 'gm');
} catch (err) {
    console.error(`Invalid regex: ${regexString}`);
    process.exit(1);
}

executeRegexAndFormat(regex, inputs);

