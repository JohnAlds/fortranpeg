import { writeFileSync } from 'fs';
import path from 'path';
import nodes from './Nodes.js';

const __dirname = import.meta.dirname;
const classesDestination = '../src/Visitor/Cst.js';
const visitorDestination = '../src/Visitor/Visitor.js';

let codeString = `
export default class Visitor {
`;

for (const node of Object.keys(nodes)) {
    codeString += `\tvisit${node}(node) {}\n`;
}
codeString += `}`;

writeFileSync(path.join(__dirname, visitorDestination), codeString);

console.log('--------- Generated Visitor.js ----------');

// Generate Cst

codeString = `
import Node from './Node.js';
`;

for(const [name, args] of Object.entries(nodes)){
    codeString += `
export class ${name} extends Node{
        
        constructor(${args.join(',')}){
            super();
            ${args.map((arg) => `this.${arg} = ${arg};`).join('\n\t\t\t')}
        }

        accept(visitor){
            return visitor.visit${name}(this);
        }
    }
    `;

    console.log(`--------------- Generated Visitor.visit.${name} --------------`);
}

writeFileSync(path.join(__dirname, classesDestination), codeString);

console.log('--------- Congrulations, CST and Visitors Generated ----------')


















