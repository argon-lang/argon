package dev.argon.compiler

import zio.ZIO

sealed trait AccessToken extends UsingContext {
  val tube: TubeName
  val module: ModulePath
  val allowsAccessTo: Set[DeclarationBase & HasContext[context.type]]

  def add(decl: DeclarationBase & HasContext[context.type]): AccessToken & HasContext[context.type] = AccessToken(context)(
    tube = tube,
    module = module,
    allowsAccessTo = allowsAccessTo + decl
  )

  def allows(request: AccessRequest[context.type]): Comp[Boolean] =
    request.accessModifier match {
      case AccessModifier.Public => ZIO.succeed(true)
      case AccessModifier.ModulePrivate =>
        def isInModule(importSpec: ImportSpecifier): Boolean =
          importSpec match {
            case ImportSpecifier.Global(tube, module, _, _) => tube == this.tube && module == this.module
            case ImportSpecifier.Local(parent, _) => isInModule(parent)
          }

        request.decl.importSpecifier.map(isInModule)

      case AccessModifier.Internal =>
        def isInTube(importSpec: ImportSpecifier): Boolean =
          importSpec match {
            case ImportSpecifier.Global(tube, _, _, _) => tube == this.tube
            case ImportSpecifier.Local(parent, id) => isInTube(parent)
          }

        request.decl.importSpecifier.map(isInTube)

      case AccessModifier.Private => ZIO.succeed(allowsAccessTo.contains(request.decl))
      case AccessModifier.Protected =>
        def getParentTypes(decl: DeclarationBase & HasContext[context.type]): Comp[Seq[DeclarationBase & HasContext[context.type]]] =
          decl match {
            case decl: ArFunc => ZIO.succeed(Seq())
            case decl: ArEnum => ZIO.succeed(Seq())
            case decl: ArRecord => ZIO.succeed(Seq())
            case decl: ArTrait => ZIO.succeed(Seq())
            case decl: ArInstance =>
              decl.signature.map { sig =>
                sig.returnType match {
                  case context.DefaultExprContext.Expr.TraitType(arTrait, _) =>
                    Seq(arTrait)

                  case _ => Seq()
                }
              }
          }

        def isRelevantInstance(decl: DeclarationBase & HasContext[context.type]): Comp[Boolean] =
          ZIO.succeed(allowsAccessTo.contains(decl)) ||
            getParentTypes(decl).flatMap { parent => ZIO.exists(parent)(isRelevantInstance) }

        def checkParentTypes(parentType: DeclarationBase & HasContext[context.type]): Comp[Boolean] =
          ZIO.succeed(parentType.id == request.decl.id) ||
            getParentTypes(parentType).flatMap { parent => ZIO.exists(parent)(checkParentTypes) }


        ZIO.succeed(allowsAccessTo.contains(request.decl)) || (
          ZIO.exists(request.instanceDecl)(isRelevantInstance) &&
            ZIO.exists(allowsAccessTo)(checkParentTypes)
        )

      case AccessModifier.ProtectedOrInternal =>
        allows(request.copy(accessModifier = AccessModifier.Internal)) ||
          allows(request.copy(accessModifier = AccessModifier.Protected))

      case AccessModifier.ProtectedAndInternal =>
        allows(request.copy(accessModifier = AccessModifier.Internal)) &&
          allows(request.copy(accessModifier = AccessModifier.Protected))
    }
}

object AccessToken {
  def apply(context: Context)(tube: TubeName, module: ModulePath, allowsAccessTo: Set[DeclarationBase & HasContext[context.type]]): AccessToken & HasContext[context.type] =
    val ctx: context.type = context
    val tube2 = tube
    val module2 = module
    val allowsAccessTo2 = allowsAccessTo
    new AccessToken {
      override val context: ctx.type = ctx
      override val tube: TubeName = tube2
      override val module: ModulePath = module2
      override val allowsAccessTo: Set[DeclarationBase & HasContext[context.type]] = allowsAccessTo2
    }
  end apply
}

