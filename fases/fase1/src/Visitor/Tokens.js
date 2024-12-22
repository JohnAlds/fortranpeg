import Visitor from './Visitor.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        const code =   `
            module tokenizer
            implicit none

            contains
            function nextSym(input, cursor) result(lexeme)
                character(len=*), intent(in) :: input
                integer, intent(inout) :: cursor
                character(len=:), allocatable :: lexeme

                !declaracion de variables


                !EOF 
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
            end function nextSym
            end module tokenizer
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
        console.log(node.expr.accept(this));
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
                    console.log(node.qty)
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
        return node.expr.accept(this);
    }
    visitString(node) {
        const stringVal = node.val.toString();
        const length = stringVal.length;

        let code = `(cursor + ${node.val.length - 1} <= len(input) .and. "${node.val}" == input(cursor:cursor + ${ node.val.length - 1 }))`;


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

/*
cadena =  "hola"
 
*/