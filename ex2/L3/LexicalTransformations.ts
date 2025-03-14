import { ClassExp, ProcExp, Exp, Program, makeProcExp, makeIfExp, CExp, makeAppExp, makePrimOp, makeBoolExp, makeVarDecl, VarDecl, isClassExp, isProgram, makeDefineExp, makeVarRef, makeLitExp, isCExp, DefineExp, AppExp, IfExp, LetExp, makeLetExp, makeBinding, makeProgram, isDefineExp, isAppExp, isIfExp, isProcExp, isLetExp } from "./L3-ast";
import { Result, makeFailure, either, makeOk} from "../shared/result";
import { map } from "ramda";

/*
Purpose: Transform ClassExp to ProcExp
Signature: class2proc(classExp)
Type: ClassExp => ProcExp
*/
export const class2proc = (exp: ClassExp): ProcExp =>{
    const methids2CExp = (msg: VarDecl,index: number): CExp => {
        if( index < exp.methods.length){
            const name = exp.methods[index].var.var;
            const body = exp.methods[index].val;
             return makeIfExp(makeAppExp(makePrimOp(`eq?`),[makeVarRef(msg.var), makeLitExp(`'${name}`)]),makeAppExp(body,[]),methids2CExp(msg,++index));
        }
        return makeBoolExp(false);
    }
    const vardec = makeVarDecl("msg");
    const vars = makeProcExp([vardec],[methids2CExp(vardec,0)]);
    return makeProcExp(exp.feilds,[vars]); 
}
    //@TODO


/*
Purpose: Transform all class forms in the given AST to procs
Signature: lexTransform(AST)
Type: [Exp | Program] => Result<Exp | Program>
*/

export const lexTransform = (exp: Exp | Program): Result<Exp | Program> =>{
    
    const lexDefine = (ex : DefineExp): DefineExp =>{
        return isClassExp(ex.val) ? makeDefineExp(ex.var,class2proc(ex.val)) : ex;
    }

    const lexApp = (ex : AppExp): AppExp =>{
        return makeAppExp(isClassExp(ex.rator) ? class2proc(ex.rator) : ex.rator ,
                map(x => isClassExp(x) ? class2proc(x) : x, ex.rands));
    }

    const lexIf = (ex : IfExp): IfExp =>{
        return makeIfExp(isClassExp(ex.test) ? class2proc(ex.test) : ex.test ,
                         isClassExp(ex.then) ? class2proc(ex.then) : ex.then ,
                         isClassExp(ex.alt) ? class2proc(ex.alt) : ex.alt);
    }

    const lexProc = (ex : ProcExp): ProcExp =>{
        return makeProcExp(ex.args, map(x => isClassExp(x) ? class2proc(x) : x,ex.body))
    }

    const lexLet = (ex : LetExp): LetExp =>{
        return makeLetExp(map(x => makeBinding(x.var.var, isClassExp(x.val) ? class2proc(x.val) : x.val), ex.bindings),
                          map(x => isClassExp(x) ? class2proc(x) : x, ex.body));
    }
    const switchCase = (ex : Exp): Exp =>
        isDefineExp(ex) ? lexDefine(ex) :
        isAppExp(ex) ? lexApp(ex) :
        isIfExp(ex) ? lexIf(ex) :
        isProcExp(ex) ? lexProc(ex) :
        isLetExp(ex) ? lexLet(ex) :
        ex;

    return isProgram(exp) ? makeOk(makeProgram(map( x => switchCase(x) ,exp.exps))) :
           isClassExp(exp) ? makeOk(class2proc(exp)) : makeFailure("error"); 

}
    //@TODO
    
