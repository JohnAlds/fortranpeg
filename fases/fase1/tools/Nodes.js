const nodes = {
    Producciones: ['id', 'expr', 'alias'],
    Opciones: ['exprs'],
    Union: ['exprs'],
    Expresion: ['expr', 'label', 'qty'],
    String: ['val', 'isCase'],
    Integer : ['val','instruction'],
    Clase: ['chars'],
    Rango: ['bottom', 'top'],
};

export default nodes;