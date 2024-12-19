import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.50.0/+esm';
import { parse } from './Analyzer/Parser.js';

const resultado = document.querySelector('#resultado')

export let ids = []
export let usos = []
export let errores = []

monaco.editor.defineTheme('temaPersonalizado', {
    base: 'vs-dark',
    inherit: true,
    rules: [
        { token: 'comment', foreground: 'AAAAAA', fontStyle: 'italic' },
        { token: 'keyword', foreground: 'FF0000', fontStyle: 'bold' },
        { token: 'string', foreground: '00FF00' }
    ],
    colors: {
        'editor.background': '#1E1E1E',
        'editor.foreground': '#D4D4D4',
        'editorCursor.foreground': '#FFFFFF'
    }
});

// Tema personalizado para el editor
const editor = monaco.editor.create(
    document.getElementById('editor'), {
        value: '',
        language: 'javascript',
        theme: 'temaPersonalizado',
        automaticLayout: true,
        fontSize: 25,
        wordWrap: 'on',
        tabSize: 1,
    }
);


let decorations = [];

// Analizar contenido del editor
const analizar = () => {
    const entrada = editor.getValue();
    ids.length = 0
    usos.length = 0
    errores.length = 0
    try {
        const cst = parse(entrada)
        console.log(cst)
        if(errores.length > 0){
            resultado.innerHTML = `
            <p class=class="text-red-800 text-4xl font-bold bg-red-200 py-4 px-3 rounded-sm ">Errores: ${errores[0].message}</p>            
            `
            return
        }else{
            // Si el resultado es exitoso
            resultado.innerHTML = `
                <p class="text-green-800 text-6xl font-bold bg-green-200 py-5 px-4 rounded-2xl w-full"> Análisis Exitoso!</p>
            `
        }
        decorations = editor.deltaDecorations(decorations, []);
    } catch (e) {

        if(e.location === undefined){
            resultado.innerHTML = `
                <p class="text-red-800 text-2xl font-bold bg-red-200 py-4 px-3 rounded-sm text-4xl">
                Error: ${e.message}
                </p>
            `
        }else {
            // Mostrar mensaje de error en el editor de salida
            resultado.innerHTML = `
                <p class="text-red-800 text-2xl font-bold bg-red-200 py-4 px-3 rounded-sm text-4xl">
                Error: ${e.message}\nEn línea ${e.location.start.line} columna ${e.location.start.column}
                </p>
            `
            // Resaltar el error en el editor de entrada
            decorations = editor.deltaDecorations(decorations, [
                {
                    range: new monaco.Range(
                        e.location.start.line, 
                        e.location.start.column, 
                        e.location.start.line, 
                        e.location.start.column + 1
                    ),
                    options: {
                        inlineClassName: 'errorHighlight', // Clase CSS personalizada para cambiar color de letra
                    }
                },
                {
                    range: new monaco.Range(
                        e.location.start.line, 
                        e.location.start.column, 
                        e.location.start.line, 
                        e.location.start.column
                    ),
                    options: {
                        glyphMarginClassName: 'warningGlyph', // Clase CSS para mostrar un warning en el margen
                    }
                }
            ]);
        }
        
    }
};



// Escuchar cambios en el contenido del editor
editor.onDidChangeModelContent(() => {
    analizar();
});