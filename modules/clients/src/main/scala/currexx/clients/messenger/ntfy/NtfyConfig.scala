package currexx.clients.messenger.ntfy

final case class NtfyConfig(
    enabled: Boolean,
    baseUri: String,
    topic: String
)
