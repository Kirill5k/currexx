package currexx.core.common.http

import currexx.domain.JsonCodecs
import sttp.tapir.json.circe.TapirJsonCirce

transparent trait TapirJson extends TapirJsonCirce with JsonCodecs
