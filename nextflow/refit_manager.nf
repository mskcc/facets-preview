#!/usr/bin/env nextflow
nextflow.enable.dsl = 2

import java.util.Timer
import java.util.TimerTask

def addSamplesFromQueue() {
    new Timer().schedule({
        def command = ["sh", "-c", "mv ${params.queue_directory}/*.sh ${params.watch_directory}"]
        println("Executing command: ${command}")
        Runtime.getRuntime().exec((String[]) command.toArray())
    } as TimerTask, 1000, 10000)  // Initial delay of 1 second, repeat every 10 seconds
}

process SubmitRefit {
    errorStrategy = { task.attempt <= 3 ? 'retry' : 'ignore' }

    input:
    path script_file

    script:
    """
    chmod 775 ${script_file}
    ./${script_file}
    """
}

workflow {
    // Start the sample moving task
    addSamplesFromQueue()

    // Watch the target directory for new .sh files
    sh_files_channel = Channel
        .watchPath(params.watch_directory + "*.sh", 'create')

    // Run the SubmitRefit process for each new .sh file
    SubmitRefit(sh_files_channel)
}

