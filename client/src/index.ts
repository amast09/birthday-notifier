import ENVIRONMENT, { EnvironmentKey } from "./environment";
import api from "./api";
import scheduler from "./scheduler";

scheduler.start();

api.listen(ENVIRONMENT[EnvironmentKey.API_PORT]);
