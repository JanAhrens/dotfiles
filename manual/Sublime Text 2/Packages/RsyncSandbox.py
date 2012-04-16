import sublime, sublime_plugin, subprocess, time, os

class SyncSandboxCommand(sublime_plugin.TextCommand):


    def getSandboxCommands(self, sandbox):
        RSYNC_OPTIONS="--exclude='.git*' --exclude='htdocs/' --exclude='log/' --exclude='conf/settings*' --exclude='tmp/' --exclude='gems' --exclude '.tm_sync.config' "
        REMOTE_PORT="22"
        REMOTE_HOST="threadpost-jan-ahrens.env.xing.com"
        REMOTE_USER="vagrant"
        LOCAL_PATH ="~/projects/rails-app"
        REMOTE_PATH="~/sandbox/"

        rsync = "rsync -p -P -C -zar {0} --port {1} {2} {3}@{4}:{5}".format(RSYNC_OPTIONS, REMOTE_PORT, LOCAL_PATH, REMOTE_USER, REMOTE_HOST, REMOTE_PATH)

        return [rsync]

    def getReportView(self):
        # try to get existing sync status window
        sandbox_buffer_name = "Sandbox Sync Status"
        window = sublime.active_window()
        views = window.views()
        reportView = None

        # recycle old report view
        for view in views:
            if view.name() == sandbox_buffer_name:
                reportView = view
                break

        # create new report view if none exists
        if reportView == None:
            reportView = window.new_file()

        syntax="Packages/HTML/HTML.tmLanguage"
        reportView.set_scratch(True)
        reportView.set_syntax_file(syntax)
        reportView.set_name(sandbox_buffer_name)
        return reportView

    # execute all commands and write report into status window
    def executeSandboxCommands(self, commands, reportView = None):
        output = False

        if reportView != None:
            # clear view
            reportView.set_read_only(False)
            edit = reportView.begin_edit()
            region = sublime.Region(0, reportView.size())
            reportView.erase(edit, region)

            # execute commands
            for command in commands:
                process = subprocess.Popen([command], shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
                result = process.stdout.read()
                if result:
                    reportView.insert(edit, reportView.size(), result + '\n')
                    output = True
                process.wait()

            # get output
            if output:
                reportView.insert(edit, 0, "Latest Sandbox Sync: {0}\n\n".format(time.strftime('%x %X')))
                sublime.status_message("Latest Sandbox Sync: {0}".format(time.strftime('%X')))
            else:
                reportView.insert(edit, 0, "Sandbox Sync Failed: {0}\n\n".format(time.strftime('%x %X')))
                sublime.status_message("Sandbox Sync Failed: {0}".format(time.strftime('%X')))

            reportView.end_edit(edit)
            reportView.set_read_only(True)

        else:
            for command in commands:
                process = subprocess.Popen([command], shell=True)
                process.wait()

    def run(self, edit, sandbox="pandora", silent=False):

        if not silent:
            # open new window if there is no existing status window
            window = sublime.active_window()
            currentView = window.active_view()
            reportView = self.getReportView()
            window.focus_view(currentView)
        else:
            reportView = None

        self.executeSandboxCommands(self.getSandboxCommands(sandbox), reportView)
