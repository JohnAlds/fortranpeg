import Visitor from './Visitor.js';

export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        return `
module tokenizer
implicit none

contains
function nextSym(input, cursor) result(lexeme)
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
end function nextSym
end module tokenizer
        `;
    }

    visitProducciones(node) {
        return node.expr.accept(this);
    }
    visitOpciones(node) {
        return node.exprs[0].accept(this);
    }
    visitUnion(node) {
        return node.exprs[0].accept(this);
    }
    visitExpresion(node) {
        return node.expr.accept(this);
    }
    visitString(node) {
        
        return `
    if (cursor + ${node.val.length - 1} <= len(input) .and. "${node.val}" == input(cursor:cursor + ${
            node.val.length - 1
        })) then !Foo
        allocate( character(len=${node.val.length}) :: lexeme)
        lexeme = input(cursor:cursor + ${node.val.length - 1})
        cursor = cursor + ${node.val.length}
        return
    end if
    `;
    }

    visitInteger(node) {
        const stringVal = node.val.toString();
        const length = stringVal.length;
        return ` 
        if (${stringVal} == input(cursor:cursor + ${length - 1})) then !Foo
            allocate(character(len=${length}) :: lexeme)
            lexeme = input(cursor:cursor + ${length - 1})
            cursor = cursor + ${length}
            return
        end if
        `;
    }
    
}