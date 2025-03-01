let state ~cwd = Path.(cwd / "state.rens")
let sectors ~cwd = Path.(cwd / "list" / "sectors.rens")
let projects ~cwd = Path.(cwd / "list" / "projects.rens")
let logs ~cwd = Path.(cwd / "logs")
let all_logs ~cwd = Path.(logs ~cwd / "list")
let transient_logs ~cwd = Path.(logs ~cwd / "transient.rens")
let sector_folder ~cwd = Path.(cwd / "sectors")
let project_folder ~cwd = Path.(cwd / "projects")
let last_logs ~cwd = Path.(cwd / "last_logs.rens")
