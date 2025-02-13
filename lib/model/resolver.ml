let sectors ~cwd = Path.(cwd / "list" / "sectors.rens")
let projects ~cwd = Path.(cwd / "list" / "projects.rens")
let logs ~cwd = Path.(cwd / "logs")
let transient_logs ~cwd = Path.(logs ~cwd / "transient.rens")
