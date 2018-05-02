(deforphan arc:enforceUserTypeRules(Lambda)
;; *******************************************************************
;; summary:  The user type rules specified in the type decoration are enforced.  
;;
;;           RQL supports the declaration of user specified type rules.
;;           Violation of the user specified type rules prevents the
;;           candidate Lambda from being scored. 
;;
;;           For example, the following ARC estimator type decoration, with
;;           the specified user defined type rules, ... 
;;
;;             search regress(universal(1,1,t)) 
;;               where {fitness(nmae) 
;;                      island(pareto,standard,256,25,00,10,10,100,10,ec,bc,10,2) 
;;                      champion(standard,10,25,5,5,reduce,0.0000000001,0.0000000001) 
;;                      op(noop,inv,abs,sqroot,square,cube,curoot,quart,quroot,exp,ln,cos,sin,tan,tanh,+,-,*,/,maximum,minimum,<=,>=,lif,lor,land)  
;;                      onscore(0.0,160)
;;                      type(
;;                           hours,'x1:x2:x3:(hours+hours):(hours-hours):(hours*hours):(hours/hours):max(hours,hours)',
;;                           hours,'(hours+Number):(hours-Number):(hours*Number):(hours/Number):max(hours,Number)',
;;                           hours,'(Number+hours):(Number-hours):(Number*hours):(Number/hours):max(Number,hours)',
;;                           days,'x0:x4:x5:x6:x7:(days+days):(days-days):(days*days):(days/days):max(days,days)',
;;                           days,'(days+Number):(days-Number):(days*Number):(days/Number):max(days,Number)',
;;                           days,'(Number+days):(Number-days):(Number*days):(Number/days):max(Number,days)',
;;                           Number,'(Number+Number):(Number-Number):(Number*Number):(Number/Number):max(Number,Number)'
;;                           )
;;                     }
;; 
;;           For instance the above user specified type rules will prevent any candidate containing (x1*x4) anywhere in its 
;;           estimator formula. The reason is because (x1*x4) will return a type result of (hours*days) and there is no rule
;;           allowing hours to be multiplied by days. So the Lambda will not be scored. 
;;
;;           Type rule shortcuts allow a single rule to cover many similar operators:
;;
;;             Abstract or Concrete constants (i.e. c3 or 45.23) := Number
;;             Abstract of Concrete features (i.e. x2 or v1) := the type specified for the concrete feature
;;             Returns false if ANY argument is false := lda logit mgp model nlc nomial regress select     
;;             (type+type) := + avg summarize     
;;             (type-type) := - ^-     
;;             (type*type) := * product sqroot square curoot cube quroot quart psqrt psquare pcube pquart expt  
;;             (type/type) := / ^/ inv
;;             (type=type) := < <= == != >= > land lor !
;;             lif(type,type,type) := lif(type,type,type)  
;;             max(type,type) := max min 
;;             sin(type) := sin cos tanh tanh 
;;             abs(type) := abs 
;;             exp(type) := exp 
;;             binary(type) := binary 
;;             sig(type) := sig 
;;             ln(type) := ln 
;;
;; args:     Lambda      The scoring candidate Lambda.
;;           
;; Return:   result      The return falue of true IFF the Lambda is NOT to be scored. 
;;                           
;; Notes:    The Lambda.TYPE variable contains the user defined type Dictionary.
;;
;; *******************************************************************
  pvars:(;; Public Variables
         myLambda                   ;; The Lambda on which user specified rules are to b e enforced.              
         myUserRulesDictionary      ;; The Dictionary of user specified rules.              
         ;; Expression grammar production Rules
         evalRule                   ;; Enforce user defined type rules on this grammar production rule..
         ruleAbs                    ;; Produce a Estimator numeric absolute function such as "abs(x1)". 
         ruleAdd                    ;; Produce a Estimator numeric addition such as x1 + xm. 
         ruleAnd                    ;; Produce a Estimator relational operator such as "(x1 <= x2) && (x4 > x3)". 
         ruleAvg                    ;; Produce a Estimator numeric average such as average(x1,x2). 
         ruleColon                  ;; Produce a Estimator conditional operator such as (x1 <= x2) ? (x1 + x3) : x4 - x2.
         ruleCons                   ;; Produce a Estimator abstract numeric constant.
         ruleCos                    ;; Produce a Estimator numeric cosine function such as "cos(x1)". 
         ruleCube                   ;; Produce a Estimator numeric cube function such as "(x1*x1*x1)". 
         ruleCurt                   ;; Produce a Estimator numeric cube root function such as "curoot(x1)". 
         ruleDiv                    ;; Produce a Estimator numeric protected division such as pdiv(x1,xm). (x3 == 0 ? x4 : x4 / x3) 
         ruleDivR                   ;; Produce a Estimator numeric protected reverse division such as pdiv(x1,xm). (x3 == 0 ? x4 : x4 / x3) 
         ruleEq                     ;; Produce a Estimator relational operator such as "(x1 == x2)". 
         ruleExp                    ;; Produce a Estimator exponential function such as "exp(x2)".
         ruleExpression             ;; Produce a Estimator genetic programming numeric expressions such as "((x1-x6)*cos(x2))". 
         ruleExpt                   ;; Produce a Estimator exponent function such as "expt(abs(x1),x2)".
         ruleBinary                 ;; Produce a Estimator numeric binary function such as "binary(x1)". 
         ruleGt                     ;; Produce a Estimator relational operator such as "(x1 > x2)". 
         ruleGte                    ;; Produce a Estimator relational operator such as "(x1 >= x2)". 
         ruleIf                     ;; Produce a Estimator conditional operator such as "((x1>0.0) ? x2 : 0.0)". 
         ruleInv                    ;; Produce a Estimator inversion function such as "(1.0 /x1)". 
         ruleLda                    ;; Produce a Estimator nonlinear classifier statement such as "lda(x3,x4);". 
         ruleLn                     ;; Produce a Estimator numeric ln function such as "ln(abs(x1))". 
         ruleLogit                  ;; Produce a Estimator logistic regression statement such as "logit(x3,sin(x10)) 
         ruleLt                     ;; Produce a Estimator relational operator such as "(x1 < x2)". 
         ruleLte                    ;; Produce a Estimator relational operator such as "(x1 <= x2)". 
         ruleMax                    ;; Produce a Estimator numeric maximum such as maximum(x1,x2,...,xm). 
         ruleMdl                    ;; Produce a Estimator model statement such as "model x3*x4;". 
         ruleMgp                    ;; Produce a Estimator multinomial classifier statement such as "mgp(x3,x4);". 
         ruleMin                    ;; Produce a Estimator numeric minimum such as minimum(x1,x2,...,xm). 
         ruleMod                    ;; Produce a Estimator numeric mod such as mod(x1,xm). 
         ruleMul                    ;; Produce a Estimator numeric multiplication such as x1 * xm. 
         ruleMvl                    ;; Produce a Estimator multiple linear regression statement such as "regress (.23*x3,34.5*x4);" WITH AXIS CONSTANT. 
         ruleName                   ;; Produce a Estimator element name such as xtime, or x1 thru xM.
         ruleNeg                    ;; Produce a Estimator numeric negation such as -(x1 * xm). 
         ruleNeq                    ;; Produce a Estimator relational operator such as "(x1 != x2)". 
         ruleNlc                    ;; Produce a Estimator nonlinear classifier statement such as "ncl(x3,x4);". 
         ruleNodes                  ;; Produce a Estimator statement containing the actively referenced variables as a series, i.e. v0,v3,v7,v10,...
         ruleNoise                  ;; Produce a Estimator abstract random noise reference such as e0, or e1.
         ruleNom                    ;; Produce a Estimator nomial statement such as "nomial(x3,x4);". 
         ruleNop                    ;; Produce a Estimator no operation.
         ruleNot                    ;; Produce a Estimator lnical not function such as "!(x1 <= x2)". 
         ruleOr                     ;; Produce a Estimator relational operator such as "(x1 <= x2) || (x4 > x3)". 
         rulePolyr                  ;; Produce a Estimator numeric multiplication such as (x1*sqroot(xm)). 
         rulePoly2                  ;; Produce a Estimator numeric multiplication such as (x1*square(xm)). 
         rulePoly3                  ;; Produce a Estimator numeric multiplication such as (x1*cube(xm)). 
         rulePoly4                  ;; Produce a Estimator numeric multiplication such as (x1*quart(xm)). 
         ruleProd                   ;; Produce a Estimator numeric product such as summarize(x1*x2*...*xm). 
         ruleQ                      ;; Produce a Estimator conditional operator such as (x1 <= x2) ? (x1 + x3) : x4 - x2. 
         ruleQurt                   ;; Produce a Estimator numeric quart root function such as "quroot(x1)". 
         ruleQuart                  ;; Produce a Estimator quart function such as "(x1*x1*x1*x1)". 
         ruleReg                    ;; Produce a Estimator regress statement such as "regress x3*x4;" WITH AXIS CONSTANT. 
         ruleRnop                   ;; Produce a Estimator right no operation.
         ruleRop                    ;; Produce a Estimator abstract relational operator reference such as r0(x1,c0), or r1(c0,x2) thru rK(v1,v2).
         ruleSelect                 ;; Produce a Estimator numeric select function call such as select(x1,x2,...,xm). 
         ruleSigmoid                ;; Produce a Estimator numeric sigmoid function such as "sig(x1)". 
         ruleSign                   ;; Produce a Estimator numeric sign function such as "sign(x1)". 
         ruleSin                    ;; Produce a Estimator numeric sine function such as "sin(x1)". 
         ruleSqrt                   ;; Produce a Estimator numeric square root function such as "sqroot(x1)". 
         ruleSquare                 ;; Produce a Estimator numeric square function such as "(x1*x1)". 
         ruleSub                    ;; Produce a Estimator numeric subtraction such as x1 - xm. 
         ruleSubR                   ;; Produce a Estimator numeric reverse subtraction such as x1 - xm. 
         ruleSum                    ;; Produce a Estimator numeric sum such as summarize(x1,x2,...,xm). 
         ruleTan                    ;; Produce a Estimator numeric tangent function such as "tan(x1)". 
         ruleTanh                   ;; Produce a Estimator numeric hyper tangent function such as "tanh(x1)". 
         ruleTerm                   ;; Produce a Estimator abstract term reference such as t0, or t1 thru tK.
         ruleVar                    ;; Produce a Estimator abstract variable reference such as v0, or v1 thru vK.
         ruleVop                    ;; Produce a Estimator abstract operator reference such as f0(x1), or f1(c0,x2) thru fK(v1,v2).
         ) ; end persistent variables

    ;; *******************************************************************************
    ;; *******************************************************************************
    ;; Define Expression grammar user type enforcement Rules 
    ;; *******************************************************************************
    ;; *******************************************************************************

    (defun evalRule(rule ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     rule        Any ARC grammar rule to be evaluated.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
      vars:(ruleHdr result num key elementID functionID funName)
      (onError (lambda(msg) (writeln "arc.enforceUserTypeRules.evalRule: " msg ) false))
      (if (= rule #void) (return true))
      (if (isString rule) (setq rule (arc.listWff rule)))
      (if (isPair rule) 
          (begin
           (setq result (apply (setq ruleHdr evalRule[(car rule)]) (cdr rule)))
           (if (= result #void) then (setq result false))
          ) else
          (begin
           (cond 
            ((isNumber rule) 
             (setq result Number:)
             ) ; end case real constant
            ((and (isSymbol rule) (= rule[0] #\c) (isCharNumeric (setq num (mid rule 1 1000))))
             (setq result Number:)
             ) ; end case abstract constant
            ((and (isSymbol rule) (= rule[0] #\x) (isCharNumeric (setq num (mid rule 1 1000))))
             (setq elementID (parse num))
             (setq key (symbol (append "x" elementID)))
             (setq result myUserRulesDictionary[key])
             ) ; end case concrete variable name
            ((and (isSymbol rule) (= rule[0] #\v) (isCharNumeric (setq num (mid rule 1 1000))))
             (setq elementID (parse num))
             (setq elementID myLambda.VV[elementID])         
             (setq key (symbol (append "x" elementID)))
             (setq result myUserRulesDictionary[key])
             ) ; end case abstract variable name
            (else (writeln "arc.enforceUserTypes.evalRule: unknown rule [" rule "]"))
            ) ; end cond
           (if (= result #void) then (setq result false))
          )) ; end if
      result) ; end evalRule

    (defun ruleAbs(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "abs(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleAbs    
    
    (defun ruleAdd(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "+" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleAdd    
        
    (defun ruleAnd(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleAnd        
        
    (defun ruleAvg(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq argc (argCount)) 
       (setq key (symbol (append "(" targ1 "+" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (return false))
       (loop for n from 2 until argc do
         (setq targ2 result)
         (setq targ2 (evalRule (argFetch n)))
         (setq key (symbol (append "(" targ1 "+" targ2 ")")))
         (setq result myUserRulesDictionary[key])
         (if (= result #void) then (return false))
         ) ; end loop
       result) ; end ruleAvg    
       
    (defun ruleBinary(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "binary(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleBinary    
        
    (defun ruleColon(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;           
    ;; Note:     The ruleColon operator functions as a contitional operator
    ;;           only in the context of the ruleQ operator.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(result)
       ;; Return the right most of the specified WFFs.
       (setq result (evalRule wff2))  
       result) ; end ruleColon    
    
    (defun ruleCons(Integer:constantID)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     constantID  An Integer in the range 0 thru K.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
        Number:) ; end ruleCons  
        
    (defun ruleCos(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "sin(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleCos    
        
    (defun ruleCube(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "*" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleCube    
        
    (defun ruleCurt(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "*" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleCurt    
        
    (defun ruleDiv(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "/" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleDiv
        
    (defun ruleDivR(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "/" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleDivR
        
    (defun ruleEq(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleEq
        
    (defun ruleExpression(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(result)
       (setq result (evalRule wff1))
       (if (= result #void) then (setq result false))
       result) ; end ruleExpression
        
    (defun ruleExp(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff      The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "exp(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleExp    
        
    (defun ruleExpt(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "*" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleExpt

    (defun ruleGt(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleGt
        
    (defun ruleGte(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
   ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleGte
        
    (defun ruleIf(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;           
    ;; Note:     lif((x1>0.0),x2,0.0)
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(result key wff3 targ1 targ2 targ3 argc)
       (setq wff3 (if (> (argCount) 2) then (argFetch 2) else 0.0))
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq targ3 (evalRule wff3))
       (setq key (symbol (append "lif(" targ1 "," targ2  "," targ3 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleIf    
        
    (defun ruleInv(wff ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff         A Estimator numeric WFF.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "/" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleInv            expression) ; end ruleInv    
        
    (defun ruleLda(genome ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     genome      The sparse vector of numeric expressions for the ncl statement.
    ;;           where       (Optional) The where Structure for the lda statement.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleLda    
    
    (defun ruleLn(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "ln(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleLn   
        
    (defun ruleLogit(genome ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     genome      The sparse vector of numeric expressions for the regress statement.
    ;;           where       (Optional) The where Structure for the model statement.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleLogit    

                
    (defun ruleLt(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleLt
                
    (defun ruleLte(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleLte
        
    (defun ruleMax(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq argc (argCount)) 
       (setq key (symbol (append "max(" targ1 "," targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (return false))
       (loop for n from 2 until argc do
         (setq targ2 result)
         (setq targ2 (evalRule (argFetch n)))
         (setq key (symbol (append "max(" targ1 "," targ2 ")")))
         (setq result myUserRulesDictionary[key])
         (if (= result #void) then (return false))
         ) ; end loop
       result) ; end ruleMax    
    
    (defun ruleMdl(genome ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     genome      The sparse vector of numeric expressions for the model statement.
    ;;           where       (Optional) The where Structure for the model statement.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleMdl    
        
    (defun ruleMgp(genome ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     genome      The sparse vector of numeric expressions for the model statement.
    ;;           where       (Optional) The where Structure for the model statement.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleMgp    
        
    (defun ruleMin(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq argc (argCount)) 
       (setq key (symbol (append "max(" targ1 "," targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (return false))
       (loop for n from 2 until argc do
         (setq targ2 result)
         (setq targ2 (evalRule (argFetch n)))
         (setq key (symbol (append "max(" targ1 "," targ2 ")")))
         (setq result myUserRulesDictionary[key])
         (if (= result #void) then (return false))
         ) ; end loop
       result) ; end ruleMin    
        
    (defun ruleMod(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "/" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleMod
        
    (defun ruleMul(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "*" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleMul
        
    (defun ruleMvl(genome ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     genome      The sparse vector of numeric expressions for the regress statement.
    ;;           where       (Optional) The where Structure for the model statement.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleMvl    
        
    (defun ruleName(elementID)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     elementID   An Integer in the range 0 thru M.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(result key)
       (setq name (symbol (append "x" elementID)))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleName    
        
    (defun ruleNeg(wff ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff         A Estimator numeric WFF.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "-" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleNeg   
    
    (defun ruleNeq(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleNeq
        
    (defun ruleNlc(genome ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     genome      The sparse vector of numeric expressions for the ncl statement.
    ;;           where       (Optional) The where Structure for the ncll statement.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleNlc    
                
    (defun ruleNodes(wff ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff         A Estimator numeric WFF.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "nodes(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleNodes   
        
    (defun ruleNom(genome ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     genome      The sparse vector of numeric expressions for the model statement.
    ;;           where       (Optional) The where Structure for the model statement.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleNlc    
        
    (defun ruleNop(wff ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff         A Estimator numeric WFF.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(result)
       ;; Return the right most of the specified WFFs.
       (setq result (evalRule wff))  
       result) ; end ruleNop    
        
    (defun ruleNot(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "=" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleNot   
      
    (defun ruleOr(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleLor
        
    (defun rulePolyr(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   source   The resulting numeric Estimator WFF expression
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "*" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end rulePolyr   
        
    (defun rulePoly2(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   source   The resulting numeric Estimator WFF expression
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "*" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end rulePoly2   
       
    (defun rulePoly3(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   source   The resulting numeric Estimator WFF expression
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "square(" targ1 "*" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end rulePoly3   
        
    (defun rulePoly4(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   source   The resulting numeric Estimator WFF expression
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "square(" targ1 "*" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end rulePoly4   
        
    (defun ruleProd(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq argc (argCount)) 
       (setq key (symbol (append "(" targ1 "*" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (return false))
       (loop for n from 2 until argc do
         (setq targ2 result)
         (setq targ2 (evalRule (argFetch n)))
         (setq key (symbol (append "(" targ1 "*" targ2 ")")))
         (setq result myUserRulesDictionary[key])
         (if (= result #void) then (return false))
         ) ; end loop
       result) ; end ruleProd    
        
    (defun ruleQ(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "=" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleQ
        
    (defun ruleQuart(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "*" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleQuart    
    
    (defun ruleQurt(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "*" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleQurt    
        
    (defun ruleReg(genome ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     genome      The sparse vector of numeric expressions for the regress statement.
    ;;           where       (Optional) The where Structure for the model statement.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleReg    
        
    (defun ruleRnop(wff ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff         A Estimator numeric WFF.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       ;; Return the specified WFF.
       (if (> (argCount) 1) (setq wff (argFetch 1)))
       (evalRule wff)) ; end ruleRnop    

    (defun ruleRop(functionID wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     functionID   The first expression.
    ;;           wff1         The first expression.
    ;;           wff2         (Optional) The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(wff2 result funName)
       (if (> (argCount) 2) then (setq wff2 (argFetch 2)))
       (setq functionID myLambda.FF[functionID])
       (setq funName arc.myGsoAbstractOperators[functionID])
       (setq result (arc.expressionGrammarGsomyUserRulesDictionary[funName] wff1 wff2))
       (if (= result #void) then (setq result false))
       result) ; end ruleRop
    
    (defun ruleSelect(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(result)
       (setq argc (length genome)) 
       (loop for n from 0 until argc do
         (setq result (evalRule genome[n]))
         (if (or (= result #void) (= result false)) then (return false))
         ) ; end loop
       result) ; end ruleSelect    
    
    (defun ruleSigmoid(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "sig(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleSigmoid    
        
    (defun ruleSign(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "sign(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleSign    
    
    
    (defun ruleSin(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "sin(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleSin    
    
    (defun ruleSqrt(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "*" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleSqrt    
        
    (defun ruleSquare(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "(" targ "*" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleSquare   
        
    (defun ruleSub(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "-" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleSub    
        
    (defun ruleSubR(wff1 wff2)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq key (symbol (append "(" targ1 "-" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleSubR    
        
    (defun ruleSum(wff1 wff2 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           wff2     The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       regs:(n argc)
       vars:(targ1 targ2 key result)
       (setq targ1 (evalRule wff1))
       (setq targ2 (evalRule wff2))
       (setq argc (argCount)) 
       (setq key (symbol (append "(" targ1 "+" targ2 ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (return false))
       (loop for n from 2 until argc do
         (setq targ2 result)
         (setq targ2 (evalRule (argFetch n)))
         (setq key (symbol (append "(" targ1 "+" targ2 ")")))
         (setq result myUserRulesDictionary[key])
         (if (= result #void) then (return false))
         ) ; end loop
       result) ; end ruleSum    
        
    (defun ruleTan(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "sin(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleTan    
        
    (defun ruleTanh(wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     wff1     The first expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(targ key result)
       (setq targ (evalRule wff1))
       (setq key (symbol (append "sin(" targ ")")))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleTanh    
        
    (defun ruleTerm(elementID)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     elementID   An Integer in the range 0 thru K.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(result key constantSW)
       (setq constantSW myLambda.TT[elementID])
       (if (= constantSW 1) then (return Number:))
       (setq elementID myLambda.VV[elementID])
       (setq key (symbol (append "x" elementID)))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleTerm
        
    (defun ruleVar(elementID)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     elementID   An Integer in the range 0 thru K.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(result key)
       (setq elementID myLambda.VV[elementID])
       (setq key (symbol (append "x" elementID)))
       (setq result myUserRulesDictionary[key])
       (if (= result #void) then (setq result false))
       result) ; end ruleVar
        
    (defun ruleVop(functionID wff1 ...)
    ;; *******************************************************************
    ;; summary:  Enforce user defined type rules on this grammar production rule.
    ;;
    ;; args:     functionID        The first expression.
    ;;           wff1             The first expression.
    ;;           wff2             (Optional) The second expression.
    ;;           
    ;; Return:   result   If true the user type rules allow this expression, false otherwise.
    ;;
    ;; *******************************************************************
       vars:(wff2 result funName)
       (if (> (argCount) 2) then (setq wff2 (argFetch 2)))
       (setq functionID myLambda.FF[functionID])
       (setq funName arc.myGsoAbstractOperators[functionID])
       (setq result (arc.expressionGrammarGsomyUserRulesDictionary[funName] wff1 wff2))
       (if (= result #void) then (setq result false))
       result) ; end ruleVop
    
    ;; *******************************************************************
    ;; Begin main logic
    ;; *******************************************************************
        vars:(result wff)
        (onError (lambda(err) (writeln err) false))
        ;; Convert the concrete ARC estimator expression into a WFF grammar rule List.
        (setq wff Lambda.WFF) 
        (if (isString wff) then (setq wff (arc.listWff wff)))
        (if (not (isPair wff)) then (error "arc.enforceUserTypeRules: estimator WFF not in valid format")) 
        ;; If there are no user specified type rules, then return the Lambda as having passed.
        (if (<= (length Lambda.TYPE) 0) then (return true))
        (setq myLambda Lambda)
        (setq myUserRulesDictionary Lambda.TYPE)
        ;; Check the WFF grammar expression rules to make sure they pass all user specified type rules.
        (setq result (evalRule wff))
        (if (and (<> result #void) (<> result false)) then (setq result true))
        result) ; end enforceUserTypeRules