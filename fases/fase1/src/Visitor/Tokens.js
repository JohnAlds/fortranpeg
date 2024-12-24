import Visitor from './Visitor.js';
import { generateCaracteres } from '../Utils/Util.js';
import { Rango } from './Cst.js';

let nodobottom = 0;
let nodotop = 0;
let nodobottomUppercase;
let nodotopUppercase;

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        const code =   `
            module parser
                implicit none

            contains

            subroutine parse(input)
                character(len=*), intent(in) :: input  ! Declaración de tipo correcta para input
                integer :: cursor = 1
                character(len=:), allocatable :: lexeme  ! Lexema como variable de longitud dinámica

                ! Inicializar lexeme con valor vacío
                lexeme = ""

                do while (lexeme /= "EOF" .and. lexeme /= "ERROR")
                    lexeme = nextsym(input, cursor)  ! Llamada a nextsym
                    print *, "Lexema: ", lexeme
                end do
            end subroutine parse

            function nextsym(input, cursor) result(lexeme)
                character(len=*), intent(in) :: input
                integer, intent(inout) :: cursor
                character(len=:), allocatable :: lexeme
            
                

                if (cursor > len(input)) then
                    allocate(character(len=3) :: lexeme)
                    lexeme = "EOF"
                    return
                end if
    
                
                ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

                print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
                allocate(character(len=5) :: lexeme)
                lexeme = "ERROR"
                cursor = cursor + 1  ! Avanzar el cursor para continuar procesando
            end function nextsym

            end module parser
        `;

        return code;
    }

    visitProducciones(node) {
        return node.expr.accept(this);
    }
    visitOpciones(node) {
        //console.log(node.exprs);
        return node.exprs[0].accept(this);
    }
    visitUnion(node) {
        //console.log(node.exprs);
        return node.exprs[0].accept(this);
    }
    visitExpresion(node) {
        
         //console.log(node.expr.constructor.name);
         if(node.expr.constructor.name == "String"){
            if(node.qty){
                if(node.qty == "*"){ //hola = "hola"*
                    return `
                        if ${ node.expr.accept(this).code} then
                            do while ( .TRUE.)
                                if ${ node.expr.accept(this).code} then
                                    lexeme = lexeme // input(cursor:cursor + ${ node.expr.accept(this).length - 1})
                                    cursor = cursor + ${ node.expr.accept(this).length }
                                else
                                    return
                                end if
                            end do
                        end if
                    `
                }else if(node.qty == "+"){
                    return `
                        if ${ node.expr.accept(this).code} then
                            do while ( .TRUE.)
                                if ${ node.expr.accept(this).code} then
                                    lexeme = lexeme // input(cursor:cursor + ${ node.expr.accept(this).length - 1})
                                    cursor = cursor + ${ node.expr.accept(this).length }
                                else
                                    return
                                end if
                            end do
                        end if
                    `
                }else if(node.qty == "?"){
                    return `
                    if ${node.expr.accept(this).code} then
                        allocate( character(len=${node.expr.accept(this).length}) :: lexeme)
                        lexeme = input(cursor:cursor + ${node.expr.accept(this).length - 1})
                        cursor = cursor + ${node.expr.accept(this).length }
                        return
                    end if
                `
                }else{
                    const c = count(node.qty);
                    const exp = node.expr.accept(this);
                    
                    if(c.case == 0){ // "hola"|2|
                        let val = exp.stringVal.repeat(parseInt(c.value, 10));
                        const condicion = `(cursor + ${val.length - 1} <= len(input) .and. &\n "${val}" == input(cursor:cursor + ${val.length - 1}))`;
                        return `
                            if ${condicion} then
                                lexeme = "${val}"
                                cursor = cursor + ${val.length}
                                return
                            end if
                        `                        
                    }else if(c.case == 1){ // "hola"|2..3|
                        let val = exp.stringVal.repeat(parseInt(c.value1, 10));
                        const condicion = `(cursor + ${val.length - 1} <= len(input) .and. &\n "${val}" == input(cursor:cursor + ${val.length - 1}))`;
                        return `
                            if ${condicion} then
                                lexeme = "${val}"
                                cursor = cursor + ${val.length}
                                do while ("${exp.stringVal}" == input(cursor:cursor + ${exp.length - 1}) .and. LEN(lexeme) < ${parseInt(c.value2, 10)*exp.stringVal.length} )
                                    lexeme = lexeme  // "${exp.stringVal}"
                                    cursor = cursor + ${exp.length}
                                end do
                                return
                            end if
                        `  
                    }else if(c.case == 2){// "hola"|2..|
                        let val = exp.stringVal.repeat(parseInt(c.value1, 10));
                        const condicion = `(cursor + ${val.length - 1} <= len(input) .and. &\n "${val}" == input(cursor:cursor + ${val.length - 1}))`;
                        return `
                            if ${condicion} then
                                lexeme = "${val}"
                                cursor = cursor + ${val.length}
                                do while ("${exp.stringVal}" == input(cursor:cursor + ${exp.length - 1})  )
                                    lexeme = lexeme  // "${exp.stringVal}"
                                    cursor = cursor + ${exp.length}
                                end do
                                return
                            end if
                        `  
                    }else if(c.case == 3){// "hola"|..3|
                        let val = exp.stringVal.repeat(parseInt(1, 10));
                        const condicion = `(cursor + ${val.length - 1} <= len(input) .and. &\n "${val}" == input(cursor:cursor + ${val.length - 1}))`;
                        return `
                            if ${condicion} then
                                lexeme = "${val}"
                                cursor = cursor + ${val.length}
                                do while ("${exp.stringVal}" == input(cursor:cursor + ${exp.length - 1}) .and. LEN(lexeme) < ${parseInt(c.value2, 10)*exp.stringVal.length} )
                                    lexeme = lexeme  // "${exp.stringVal}"
                                    cursor = cursor + ${exp.length}
                                end do
                                return
                            end if
                        `  
                    }else if(c.case == 4){// "hola"|..|
                        return `
                            if ${ node.expr.accept(this).code} then
                                do while ( .TRUE.)
                                    if ${ node.expr.accept(this).code} then
                                        lexeme = lexeme // input(cursor:cursor + ${ node.expr.accept(this).length - 1})
                                        cursor = cursor + ${ node.expr.accept(this).length }
                                    else
                                        return
                                    end if
                                end do
                            end if
                        `
                    }else if(c.case == 5){

                    }else if(c.case == 6){
                        
                    }else if(c.case == 7){

                    }else if(c.case == 8){

                    }else if(c.case == 9){

                    }
                }
            }else{
                return `
                    if ${node.expr.accept(this).code} then
                        allocate( character(len=${node.expr.accept(this).length}) :: lexeme)
                        lexeme = input(cursor:cursor + ${node.expr.accept(this).length - 1})
                        cursor = cursor + ${node.expr.accept(this).length }
                        return
                    end if
                `
            }
        }
        // Empezando Rango
        if(node.expr.constructor.name == "Clase"){
            if(node.qty){
                if(node.qty == "*"){
                    return`
                    if (cursor + 1 <= len(input) .and. ${node.expr.accept(this).replace(/\s+/g, ' ')}) then 
                            lexeme = input(cursor:cursor)
                            cursor = cursor + 1
                            do while (cursor <= len(input) .and. ${ node.expr.accept(this).replace(/\s+/g, ' ')} )
                                    lexeme = lexeme // input(cursor:cursor)
                                    cursor = cursor +  1
                            end do
                            return
                    end if
                    `
                }else if(node.qty == "+"){
                    return`
                    if (cursor + 1 <= len(input) .and. ${node.expr.accept(this).replace(/\s+/g, ' ')}) then
                            lexeme = input(cursor:cursor)
                            cursor = cursor + 1
                            do while (cursor <= len(input) .and. ${ node.expr.accept(this).replace(/\s+/g, ' ')} )
                                    lexeme = lexeme // input(cursor:cursor)
                                    cursor = cursor +  1
                            end do
                            return
                    end if
                    `;
                }else if(node.qty == "?"){
                    
                    return `
                    if ${node.expr.accept(this).replace(/\s+/g, ' ')} then
                        lexeme = input(cursor:cursor)
                        cursor = cursor + 1
                        return
                    end if
                `;
                // Sino es * + ? entonces es un conteo
                }else if(node.qty == "i"){
                    return `
                          if(cursor+1 <= len(input) .and. &
                                 (${node.expr.accept(this).replace(/\s+/g, ' ')} .or. &
                                (input(cursor:cursor) >= "${nodobottomUppercase}" .and. input(cursor:cursor) <= "${nodotopUppercase}"))) then

                          lexeme = input(cursor:cursor)
                          cursor = cursor + 1

                            do while(cursor <= len(input) .and. &
                                (${node.expr.accept(this).replace(/\s+/g, ' ')} .or. &
                                (input(cursor:cursor) >= "${nodobottomUppercase}" .and. input(cursor:cursor) <= "${nodotopUppercase}")))
                                lexeme = lexeme // input(cursor:cursor)
                                cursor = cursor + 1
                            end do
                            return
                        end if

                    `;
                }else if(count(node.qty)){
                    const c = count(node.qty);
                    if(c.case == 1){
                        return `
                        if (cursor + 1 <= len(input) .and. ${node.expr.accept(this).replace(/\s+/g, ' ')} .and. &
                                (input(cursor + 1:cursor + 1) >= "${nodobottom}" .and. input(cursor + 1:cursor + 1) <= "${nodotop}")) then
                                lexeme = input(cursor:cursor)
                                lexeme = lexeme // input(cursor + 1: cursor + 1)
                                cursor = cursor + 2

                            do while (cursor <= len(input) .and. input(cursor:cursor) >= "${nodobottom}" .and. input(cursor:cursor) <= "${nodotop}" .and. LEN(lexeme) < ${c.value2} )
                                    lexeme = lexeme // input(cursor:cursor)
                                    cursor = cursor + 1
                            end do

                            if(LEN(lexeme) < ${c.value1}) then
                                print *, "error lexico en col ", cursor - LEN(lexeme), ', "'//lexeme//'"'
                                allocate(character(len=5) :: lexeme)
                                lexeme = "ERROR"
                                return

                            else if(LEN(lexeme) > ${c.value2}) then
                                print *, "error lexico en col ", cursor - LEN(lexeme), ', "'//lexeme//'" (demasiado largo)"'
                                allocate(character(len=5) :: lexeme)
                                lexeme = "ERROR"
                                return
                            end if
                            return
                        end if
                        `;
                    }else if(c.case == 2){
                        return `
                    if (cursor + 1 <= len(input) .and. ${node.expr.accept(this).replace(/\s+/g, ' ')} .and. &
                                (input(cursor + 1:cursor + 1) >= "${nodobottom}" .and. input(cursor + 1:cursor + 1) <= "${nodotop}")) then
                                lexeme = input(cursor:cursor)
                                lexeme = lexeme // input(cursor + 1: cursor + 1)
                                cursor = cursor + 2

                            do while (cursor <= len(input) .and. input(cursor:cursor) >= "${nodobottom}" .and. input(cursor:cursor) <= "${nodotop}")
                                    lexeme = lexeme // input(cursor:cursor)
                                    cursor = cursor + 1
                            end do

                            if(LEN(lexeme) < ${c.value1}) then
                                print *, "error lexico en col ", cursor - LEN(lexeme), ', "'//lexeme//'"'
                                allocate(character(len=5) :: lexeme)
                                lexeme = "ERROR"
                                return
                            end if
                            return
                        end if
                        `;
                    }else if(c.case == 3){
                        return `
                            if (cursor + 1 <= len(input) .and. ${node.expr.accept(this).replace(/\s+/g, ' ')} ) then
                            lexeme = input(cursor:cursor)
                            cursor = cursor + 1
                            
                            do while (cursor <= len(input) .and. input(cursor:cursor) >= "${nodobottom}" .and. input(cursor:cursor) <= "${nodotop}" .and. LEN(lexeme) < ${c.value2} )
                                lexeme = lexeme // input(cursor:cursor)
                                cursor = cursor + 1
                            end do
                            return
                        end if
                        `
                    }else if(c.case == 4){
                        return `
                        if (cursor + 1 <= len(input) .and. ${node.expr.accept(this).replace(/\s+/g, ' ')} ) then
                            lexeme = input(cursor:cursor)
                            cursor = cursor + 1
                            do while (cursor <= len(input) .and. input(cursor:cursor) >= "${nodobottom}" .and. input(cursor:cursor) <= "${nodotop}")
                                lexeme = lexeme // input(cursor:cursor)
                                cursor = cursor + 1
                            end do
                            return
                        end if
                        `
                    }
                }   
            }else{
                return `
                if ${node.expr.accept(this).replace(/\s+/g, ' ')} then
                    lexeme = input(cursor:cursor)
                    cursor = cursor + 1
                    return
                end if
            `;
            }
        }
        return node.expr.accept(this);
    }  

    visitString(node) {
        const stringVal = node.val.toString();
        const length = stringVal.length;

        let code = `(cursor + ${node.val.length - 1} <= len(input) .and. "${node.val}" == input(cursor:cursor + ${ node.val.length - 1 }))`;
        //console.log(code);

        return {
            variables: ``,
            stringVal,
            length,
            code
        };
    }

    visitClase(node) {
        console.log(node.chars);
        return `
                ${generateCaracteres(node.chars.filter((node) => typeof node === 'string'))}
                ${node.chars
                    .filter((node) => node instanceof Rango)
                    .map((range) => range.accept(this))
                    .join('\n')}
        `;
    }
    
    visitRango(node) {
        nodobottom = node.bottom;
        nodotop = node.top;
        nodobottomUppercase = node.bottom.toUpperCase();
        nodotopUppercase = node.top.toUpperCase();
        return `(input(cursor:cursor) >= "${node.bottom}" .and. input(cursor:cursor) <= "${node.top}")`;
    }    
}

const count = (cad) =>{
    //|2|                   valor retorno 0
    //|2..3|                valor retorno 1
    //|2..|                 valor retorno 2
    //|..3|                 valor retorno 3
    //|..|                  valor retorno 4
    //|2, "," / "-" |              valor retorno 5
    //|1..2, ","|           valor retorno 6
    //|1.., ","|            valor retorno 7
    //|..4, ","|            valor retorno 8
    //|.., ","|             valor retorno 9

    let cadena = cad.slice(1, -1); 

    const [conteo, opciones ] = dividir(cadena);

    //console.log(conteo, opciones);

    if (opciones == undefined){//no trae opciones "," / "." / "*"

        if (conteo.includes("..")){ //incluye 2 puntos
            const [ num1, num2 ] = conteo.split("..");
            console.log(num1, num2);  

            //|2..3|                valor retorno 1
            if( num1 != "" && num2 != "" ){
                return {
                    case:  1,
                    value1: num1,
                    value2: num2
                }
            }
            //|2..|                 valor retorno 2
            if( !(num1 == "") && num2 == "" ){
                return {
                    case:  2,
                    value1: num1
                }
            }

            //|..3|                 valor retorno 3
            if( num1 == "" && !(num2 == "") ){
                return {
                    case:  3,
                    value1: 0,
                    value2: num2 
                }
            }

            //|..|                  valor retorno 4
            if( num1 == "" && num2 == ""){
                return {
                    case:  4
                }
            }

        }else{
            return {
                case: 0,
                value: conteo
            }
        }


        
    }else{//trae opciones "," / "." / "*"
        if (conteo.includes("..")){ //incluye 2 puntos
            const [ num1, num2 ] = conteo.split("..");
            console.log(num1, num2);

            
            //|1..2, ","|           valor retorno 6
            if( num1 != "" && num2 != "" ){
                return {
                    case:  6,
                    value1: num1,
                    value2: num2,
                    options: extraerSimbolos(opciones)
                }
            }
            
            //|1.., ","|            valor retorno 7
            if( !(num1 == "") && num2 == "" ){
                return {
                    case:  7,
                    value1: num1,
                    options: extraerSimbolos(opciones)
                }
            }

            
            //|..4, ","|            valor retorno 8
            if( num1 == "" && !(num2 == "") ){
                return {
                    case:  8,
                    value1: 0,
                    value2: num2,
                    options: extraerSimbolos(opciones)
                }
            }

            
            //|.., ","|             valor retorno 9
            if( num1 == "" && num2 == ""){
                return {
                    case:  9,
                    value1: 0,
                    options: extraerSimbolos(opciones)
                }
            }

        }else{
            //|2, "," / "-" / "/"|              valor retorno 5
            return {
                case: 5,
                value: conteo,
                options: extraerSimbolos(opciones)
            }
        }
    }


    //regla = ""|2..|
}

function extraerSimbolos(cadena) {
    return cadena
      .split(" / ") // Divide por " / "
      .map(simbolo => simbolo.trim().slice(1, -1)); // Quita la primera y última comilla
}

function dividir(cadena) {
    const index = cadena.indexOf(","); // Encuentra el índice de la primera coma
    if (index === -1) return [cadena]; // Si no hay coma, devuelve la cadena completa en un arreglo
    return [cadena.slice(0, index), cadena.slice(index + 1)];
}