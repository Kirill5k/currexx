package io.github.kirill5k.template.common.http

import org.http4s.HttpRoutes
import sttp.tapir.generic.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce

trait Controller[F[_]] extends TapirJsonCirce with SchemaDerivation:
  def routes: HttpRoutes[F]
