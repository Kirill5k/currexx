package currexx.core.monitor

import currexx.core.ControllerSpec

class MonitorControllerSpec extends ControllerSpec {

  "A MonitorController" when {
    "POST /monitors" should {
      "create new monitor and return 201" in pending
      "return 409 if monitor for the requested currency pair already exists" in pending
    }

    "PUT /monitors/:id/pause" should {
      "pause monitor and return 204" in pending
      "error when id is invalid" in pending
      "return 404 error when monitor does not exist" in pending
    }

    "PUT /monitors/:id/resume" should {
      "pause monitor and return 204" in pending
    }

    "DELETE /monitors/:id" should {
      "delete existing monitor and return 204" in pending
    }

    "GET /monitors/:id" should {
      "find monitor by id" in pending
    }

    "GET /monitors" should {
      "return all monitors" in pending
    }
  }
}
