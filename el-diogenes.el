;;; phi-diogenes.el --- Diogenes Emacs integration -*- lexical-binding:t -*-

    ;; Copyright (C) 2022  Bruno Conte

    ;; Author: Bruno Conte <bruno@brunoc.com.br>
    ;; URL: https://github.com/brunocbr/phi-notes/
    ;; Version: 0.1.0
    ;; Package-Requires: ((emacs "27.1"))
    ;; Keywords: tlg

    ;; This file is not part of GNU Emacs.

    ;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defgroup diogenes ()
     "Integration with Diogenes"
     :group 'diogenes)

(defcustom diogenes-server-command "/usr/local/diogenes/server/diogenes-server.pl"
  "Command to start Diogenes server"
  :type 'string
  :group 'diogenes)

(defcustom diogenes-lib-path "/Applications/Diogenes.app/Contents/Resources/perl"
  "Path to Diogenes perl libraries"
  :type 'string
  :group 'diogenes)

(defcustom diogenes-corpora "tlg phi"
  "Identifiers for avaiable corpora, separated by spaces (tlg phi ddp ins chr misc bib cop)."
  :type 'string
  :group 'diogenes)


(defun diogenes-get-authors ()
  "Get author names from the corpora"
  (let ((script-command (format "env perl <<EOF
use lib '%s';
use Diogenes::Base qw(%%encoding %%context @contexts %%choices %%work %%author);
use Diogenes::Search;

my %%args;
my @types = qw(%s);
my \\$lispOutput = qq{\'( };
my \\$query;


\\$args{input_encoding} = 'Unicode';
\\$args{encoding} = 'UTF-8';
\\$args{output_format} = 'ascii';


sub show_authors
{
    my %%labels = %%{ \\$query->select_authors(select_all => 1) };
		my \\$l;
		my \\$uid;
    foreach my \\$label (keys %%labels)
    {
        \\$l = \\$labels{\\$label};
        \\$l =~ s/&1//g;
        \\$l =~ s/&//g;
        \\$l =~ s/\\\\\\\\//g;
        \\$uid = qq{ (\"\\$args{type}\" \"\\$label\") };
        \\$lispOutput = \\$lispOutput . qq{ ( \"\\$l\" \\$uid ) };
    }
}

foreach my \\$t (@types)
{
    \\$args{type} = \\$t;
    \\$query = new Diogenes::Base(%%args);
    &show_authors;
    \\$query = undef;
}

\\$lispOutput = \\$lispOutput . qq{)};

print \\$lispOutput;
EOF
" diogenes-lib-path diogenes-corpora)))
    (eval (car (read-from-string
                (shell-command-to-string script-command)
    )))))

(defun diogenes-get-works-from-author (corpus author)
  "Get works from a given `AUTHOR' in `CORPUS' database."
  (let ((script-command (format "env perl <<EOF
use lib '%%s';
use Diogenes::Base; qw(%%encoding %%context @contexts %%choices %%work %%author); 
use Diogenes::Browser;

my %%args;
my \\$type = qw{%s};
my \\$author = qw{%s};

\\$args{type} = \\$type;
\\$args{input_encoding} = 'Unicode';
\\$args{encoding} = 'UTF-8';
\\$args{output_format} = 'ascii';

my \\$lispOutput = qw{\'(};

sub show_works
{
    my %%labels = \\$query->browse_works(\\$author_num);
    foreach my \\$label (keys %%labels)
    {

        \\$lispOutput = \\$lispOutput . qw{ (\"\\$labels{\\$label}\" .  \"\\$type:\\$author_num:\\$label\") };
		}

\\$query = new Diogenes::Browser(%%args);
&show_works;
\\$lispOutput = \\$lispOutput . qw{)};
print \\$lispOutput;

EOF" corpus author)))
    (shell-command-to-string script-command)))

;; (completing-read "Select an author: " (diogenes-get-authors))
