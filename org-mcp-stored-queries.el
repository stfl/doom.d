;;; org-mcp-stored-queries.el --- Stored org-ql queries -*- lexical-binding: t -*-
;; Pre-populated from org-agenda-custom-commands in config.org.
;; Managed by org-mcp. Dynamic queries (prio-deadline>=, someday-habit)
;; have been expanded using default variable values:
;;   stfl/agenda-max-prio-group = ?D (68)
;;   stfl/agenda-backlog-prio-threshold = ?E (69)
;;   someday-habit = (or (tags \"SOMEDAY\" \"HABIT\") (habit))
;;   prio-deadline>= ?D: deadline lookahead = 12 days (fib(7)-1)
;;   prio-deadline>= ?C: deadline lookahead = 7 days (fib(6)-1)

(setq org-mcp--stored-queries
      '(("inbox" .
         ((query . "(and (not (done)) (tags \"#inbox\" \"inbox\"))")
          (description . "Inbox items (tagged #inbox or inbox, not done)")))

        ("private-today-next-actions" .
         ((query . "(and (todo \"NEXT\" \"WAIT\")
     (or (priority >= \"D\")
         (not (priority))
         (ancestors (priority >= \"D\"))
         (deadline :to 12)
         (ancestors (deadline :to 12)))
     (not (or (tags \"SOMEDAY\" \"HABIT\") (habit)))
     (not (ancestors (deadline :to 0)))
     (not (deadline :to 0))
     (not (scheduled))
     (not (primary-work)))")
          (description . "Private next actions for today (agenda \"a\" - Next Actions block, defaults expanded)")))

        ("private-today-stuck" .
         ((query . "(and (stuck-proj) (not (primary-work)))")
          (description . "Private stuck projects (no NEXT/WAIT child, not primary work)")))

        ("review-close-next" .
         ((query . "(and (todo \"NEXT\" \"WAIT\")
     (not (tags \"SOMEDAY\" \"HABIT\" \"org_jira\"))
     (not (my-habit))
     (or (not (deadline))
         (deadline :to \"+30\")
         (ancestors (deadline :to \"+30\")))
     (or (not (scheduled))
         (scheduled :to \"+30\")))")
          (description . "NEXT/WAIT items to close in review (due within 30 days)")))

        ("review-stuck" .
         ((query . "(stuck-proj)")
          (description . "All stuck projects")))

        ("tangling" .
         ((query . "(tangling)")
          (description . "Tangling TODOs (config.org tasks)")))

        ("someday-projects" .
         ((query . "(and (todo \"PROJ\")
     (or (and (priority <= \"E\")
              (not (ancestors (priority > \"E\")))
              (not (children (priority > \"E\"))))
         (tags \"SOMEDAY\")
         (children (and (todo \"NEXT\" \"WAIT\") (tags \"SOMEDAY\"))))
     (not (scheduled))
     (not (habit))
     (not (deadline)))")
          (description . "SOMEDAY/backlog projects (low priority or SOMEDAY tag)")))

        ("private-backlog" .
         ((query . "(and (or (todo \"PROJ\") (standalone-next))
     (not (primary-work))
     (not (my-habit)))")
          (description . "Private backlog: all projects and standalone next actions")))

        ("work-primary-today" .
         ((query . "(and (primary-work)
     (not (done))
     (or (my-habit)
         (deadline :to today)
         (scheduled :to today)
         (ts-active :on today)))")
          (description . "Primary work items scheduled/due today or active")))

        ("work-primary-next-actions" .
         ((query . "(and (todo \"NEXT\" \"WAIT\")
     (not (or (tags \"SOMEDAY\" \"HABIT\") (habit)))
     (not (ancestors (deadline :to 0)))
     (not (deadline :to 0))
     (not (scheduled))
     (primary-work))")
          (description . "Primary work next actions (not overdue/scheduled)")))

        ("work-primary-stuck" .
         ((query . "(and (stuck-proj) (primary-work))")
          (description . "Stuck projects in primary work")))

        ("work-secondary-today" .
         ((query . "(and (and (work) (not (primary-work)))
     (not (done))
     (or (my-habit)
         (deadline :to today)
         (scheduled :to today)
         (ts-active :on today)))")
          (description . "Non-primary work items scheduled/due today or active")))

        ("work-secondary-next-actions" .
         ((query . "(and (todo \"NEXT\" \"WAIT\")
     (or (priority >= \"C\")
         (not (priority))
         (ancestors (priority >= \"C\"))
         (deadline :to 7)
         (ancestors (deadline :to 7)))
     (not (or (tags \"SOMEDAY\" \"HABIT\") (habit)))
     (not (ancestors (deadline :to 0)))
     (not (deadline :to 0))
     (not (scheduled))
     (and (work) (not (primary-work))))")
          (description . "Non-primary work next actions (prio >= C, defaults expanded)")))

        ("work-secondary-stuck" .
         ((query . "(and (stuck-proj) (and (work) (not (primary-work))))")
          (description . "Stuck projects in non-primary work")))

        ("work-primary-backlog" .
         ((query . "(and (or (todo \"PROJ\") (standalone-next))
     (primary-work))")
          (description . "Primary work backlog: all projects and standalone next actions")))

        ("work-secondary-backlog" .
         ((query . "(and (or (todo \"PROJ\") (standalone-next))
     (and (work) (not (primary-work))))")
          (description . "Non-primary work backlog (#work tag but not primary work)")))
        ))
