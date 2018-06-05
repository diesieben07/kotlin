plugins {
    kotlin("jvm")
    id("jps-compatible")
}

dependencies {
    compileOnly(project(":idea"))
    compileOnly(project(":idea:idea-jvm"))
    compileOnly(project(":j2k"))

    compileOnly(intellijDep())
    compileOnly(intellijPluginDep("git4idea"))
}

sourceSets {
    "main" { projectDefault() }
    "test" {  }
}

ideaPlugin()
