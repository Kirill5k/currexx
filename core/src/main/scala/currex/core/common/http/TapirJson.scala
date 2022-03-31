package currex.core.common.http

import currex.core.common.JsonCodecs
import sttp.tapir.json.circe.TapirJsonCirce

transparent trait TapirJson extends TapirJsonCirce with JsonCodecs
