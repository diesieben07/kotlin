/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir.backend.js.ir

import org.jetbrains.kotlin.ir.backend.js.JsIrBackendContext
import org.jetbrains.kotlin.ir.expressions.IrExpression
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.util.OperatorNameConventions

class JsIrArithBuilder(val context: JsIrBackendContext) {

    val symbols = context.ir.symbols

    private fun buildBinaryOperator(name: Name, l: IrExpression, r: IrExpression, type: KotlinType?): IrExpression {
        val symbol = context.getOperatorByName(name, type ?: l.type)!!
        return JsIrBuilder.buildCall(symbol).apply {
            dispatchReceiver = l
            putValueArgument(0, r)
        }
    }

    private fun buildUnaryOperator(name: Name, v: IrExpression, type: KotlinType): IrExpression {
        val symbol = context.getOperatorByName(name, type)!!
        return JsIrBuilder.buildCall(symbol).apply { dispatchReceiver = v }
    }

    fun add(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.PLUS, l, r, type)

    fun sub(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.MINUS, l, r, type)

    fun mul(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.TIMES, l, r, type)

    fun div(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.DIV, l, r, type)

    fun mod(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.MOD, l, r, type)

    fun bitAnd(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.AND, l, r, type)

    fun bitOr(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.OR, l, r, type)

    fun bitXor(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(Name.identifier("xor"), l, r, type)

    fun and(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.AND, l, r, type)

    fun or(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(OperatorNameConventions.OR, l, r, type)

    fun shl(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(Name.identifier("shl"), l, r, type)

    fun shr(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(Name.identifier("shr"), l, r, type)

    fun shru(l: IrExpression, r: IrExpression, type: KotlinType? = null): IrExpression =
        buildBinaryOperator(Name.identifier("shru"), l, r, type)

    fun not(v: IrExpression): IrExpression = buildUnaryOperator(OperatorNameConventions.NOT, v, context.builtIns.booleanType)
    fun neg(v: IrExpression): IrExpression = buildUnaryOperator(OperatorNameConventions.NOT, v, v.type)
}