import Visitor from './Visitor.js';

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
        const isString = node.expr.constructor.name === "String";
        const isInteger = node.expr.constructor.name === "Integer";
    
        if (isString || isInteger) {
            const exprVal = isString ? node.expr.val : node.expr.val.toString();
            const exprLength = exprVal.length;
    
            if (node.qty) {
                const c = count(node.qty); 
    
                if (c.case === 0) { // |2|
                    let val = exprVal.repeat(parseInt(c.value, 10));
                    const condicion = `(cursor + ${val.length - 1} <= len(input) .and. &
                                        "${val}" == input(cursor:cursor + ${val.length - 1}))`;
                    return `
                        if ${condicion} then
                            allocate(character(len=${val.length}) :: lexeme)
                            lexeme = "${val}"
                            cursor = cursor + ${val.length}
                            return
                        end if
                    `;
                } else if (c.case === 1) { // |2..3|
                    let val = exprVal.repeat(parseInt(c.value1, 10));
                    const condicion = `(cursor + ${val.length - 1} <= len(input) .and. &
                                        "${val}" == input(cursor:cursor + ${val.length - 1}))`;
                    return `
                        if ${condicion} then
                            allocate(character(len=${val.length}) :: lexeme)
                            lexeme = "${val}"
                            cursor = cursor + ${val.length}
                            do while ("${exprVal}" == input(cursor:cursor + ${exprLength - 1}) .and. &
                                     LEN(lexeme) < ${parseInt(c.value2, 10) * exprLength})
                                lexeme = lexeme // "${exprVal}"
                                cursor = cursor + ${exprLength}
                            end do
                            return
                        end if
                    `;
                } else if (c.case === 2) { // |2..|
                    let val = exprVal.repeat(parseInt(c.value1, 10));
                    const condicion = `(cursor + ${val.length - 1} <= len(input) .and. &
                                        "${val}" == input(cursor:cursor + ${val.length - 1}))`;
                    return `
                        if ${condicion} then
                            allocate(character(len=${val.length}) :: lexeme)
                            lexeme = "${val}"
                            cursor = cursor + ${val.length}
                            do while ("${exprVal}" == input(cursor:cursor + ${exprLength - 1}))
                                lexeme = lexeme // "${exprVal}"
                                cursor = cursor + ${exprLength}
                            end do
                            return
                        end if
                    `;
                } else if (c.case === 3) { // |..3|
                    return `
                        do while (cursor + ${exprLength - 1} <= len(input) .and. &
                                 "${exprVal}" == input(cursor:cursor + ${exprLength - 1}) .and. &
                                 LEN(lexeme) < ${parseInt(c.value2, 10) * exprLength})
                            if (.not. allocated(lexeme)) then
                                allocate(character(len=${exprLength}) :: lexeme)
                                lexeme = input(cursor:cursor + ${exprLength - 1})
                            else
                                lexeme = lexeme // input(cursor:cursor + ${exprLength - 1})
                            end if
                            cursor = cursor + ${exprLength}
                        end do
                        if (allocated(lexeme)) then
                            return
                        end if
                    `;
                } else if (c.case === 4) { // |..|
                    return `
                        do while (cursor + ${exprLength - 1} <= len(input) .and. &
                                 "${exprVal}" == input(cursor:cursor + ${exprLength - 1}))
                            if (.not. allocated(lexeme)) then
                                allocate(character(len=${exprLength}) :: lexeme)
                                lexeme = input(cursor:cursor + ${exprLength - 1})
                            else
                                lexeme = lexeme // input(cursor:cursor + ${exprLength - 1})
                            end if
                            cursor = cursor + ${exprLength}
                        end do
                        if (allocated(lexeme)) then
                            return
                        end if
                    `;
                }
            } else {
                // Reglas de repetición: *, +, ?
                if (node.qty === "*") {
                    return `
                        do while (cursor + ${exprLength - 1} <= len(input) .and. &
                                 "${exprVal}" == input(cursor:cursor + ${exprLength - 1}))
                            if (.not. allocated(lexeme)) then
                                allocate(character(len=${exprLength}) :: lexeme)
                                lexeme = input(cursor:cursor + ${exprLength - 1})
                            else
                                lexeme = lexeme // input(cursor:cursor + ${exprLength - 1})
                            end if
                            cursor = cursor + ${exprLength}
                        end do
                        return
                    `;
                } else if (node.qty === "+") {
                    return `
                        if (cursor + ${exprLength - 1} <= len(input) .and. &
                            "${exprVal}" == input(cursor:cursor + ${exprLength - 1})) then
                            do while (cursor + ${exprLength - 1} <= len(input) .and. &
                                     "${exprVal}" == input(cursor:cursor + ${exprLength - 1}))
                                if (.not. allocated(lexeme)) then
                                    allocate(character(len=${exprLength}) :: lexeme)
                                    lexeme = input(cursor:cursor + ${exprLength - 1})
                                else
                                    lexeme = lexeme // input(cursor:cursor + ${exprLength - 1})
                                end if
                                cursor = cursor + ${exprLength}
                            end do
                            return
                        end if
                    `;
                } else if (node.qty === "?") {
                    return `
                        if (cursor + ${exprLength - 1} <= len(input) .and. &
                            "${exprVal}" == input(cursor:cursor + ${exprLength - 1})) then
                            allocate(character(len=${exprLength}) :: lexeme)
                            lexeme = input(cursor:cursor + ${exprLength - 1})
                            cursor = cursor + ${exprLength}
                            return
                        end if
                    `;
                }
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

    visitInteger(node) {
        const stringVal = node.val.toString();
        const length = stringVal.length;
        return ` 
        if ("${stringVal}" == input(cursor:cursor + ${length - 1})) then !Foo
            allocate(character(len=${length}) :: lexeme)
            lexeme = input(cursor:cursor + ${length - 1})
            cursor = cursor + ${length}
            return
        end if
        `;
    }

    visitRango(node) {
        console.log(node.val + 'comprobando entrada')
        console.log(node.isCase)
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