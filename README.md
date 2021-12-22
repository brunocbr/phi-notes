# phi-notes
 
 PHI notes emacs package

## Setup

### Package instalation

For Spacemacs, I'd recommend creating a layer under  `~/.emacs.d/private/phi`. Add the following to `packages.el`:

```lisp
(defconst phi-packages
  '((phi-notes :location local)))

(with-eval-after-load 'helm-bibtex
  (helm-bibtex-helmify-action bibtex-completion-create-phi-note helm-bibtex-create-phi-note)
  (helm-add-action-to-source'
   "Create PHI bibliographical annotation" 'helm-bibtex-create-phi-note
   helm-source-bibtex 1))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook 'phi-mode))
```

And this to `keybindings.el`:

```lisp
(spacemacs/set-leader-keys "Co" 'phi-new-originating-note)
(global-set-key (kbd "C-c )") 'helm-bibtex)
```

You may skip the lines of code for `helm-bibtex` if you're not working with bibliographical annotations (but you should).

Then, clone the package inside a local directory:

```shell
cd ~/.emacs.d/private/phi
mkdir local
cd local

git clone https://github.com/brunocbr/phi-notes.git
```

Open your `.spacemacs` configuration (`M-m f e d`) and add `phi` to `dotspacemacs-configuration-layers`. Restart emacs or reload the configuration (`M-m f e R`).

### Configure your note repository

A `.counter` file is kept in the same directory of your notes in order to keep the value of the incremental counter used for generating note ids (`0001`, `0002` etc.). To initialize it, use the interactive function `M-x phi-initialize-counter`, which will prompt for the desired note directory.

You may then configure one or multiple repositories with `customize-variable phi-repository-alist`. The list will contain a name of the repository, a corresponding directory and an optional value for the "master note" id (which is the default note displayed in the sidebar in absence of a project or parent note). This setup is also needed for the creation of independent, originating notes (`M-m C o` if you used the configuration above), when it makes sense to prompt for a location to create them. All the other functionalities are sensitive to the context of the file currently being visited.


## Usage

### Key bindings

Navigation:

| Key binding | Description                                                                                                                                    |
| ----------- | -----------                                                                                                                                    |
| `C-c ;`     | Toggle the sidebar. It will show one of the following notes: the first project note, the parent note, or the "master note" for the repository. |
| `C-c :`     | Show the sidebar and give it focus.                                                                                                            |
| `C-c u`     | Visit parent note.                                                                                                                             |
| `C-c j`     | Visit the next link to the right.                                                                                                              |

Creating notes:

| Key binding                     | Description                                                                                                                                                                                                                                                                                                                                                                              |
| -----------                     | -----------                                                                                                                                                                                                                                                                                                                                                                              |
| `M-m C o` (or other global key) | `phi-new-originating-note`. Create a new "originating" note, with no parent. It will prompt for a repository in case you have more than one configured in the `phi-repository-alist`.                                                                                                                                                                                                    |
| `C-c n d`                       | `phi-new-descendant-note`. Create a note descending from the one currently being visited. The title and a wikilink will be inserted before visiting the new note. You may prefix this command with `C-u` to visit the newly created note in another window.                                                                                                                              |
| `C-c n y`                       | `phi-yank-to-new-note`. If you have previously killed a region, you may use this command (e. g. when visiting a structure note) to create a link to a new note which will have the contents "yanked" to it. The command will prompt for a title which will be extract by default from the first line of the killed region.  This is useful for reorganizing contents in different notes. |
| `C-c n k`                       | `phi-kill-to-new-note`. Similar to the above, but may be used immediately after marking a region, which will be thus substituted with a link to a new note containing the "killed" contents. May be used e. g. when breaking a long note into more atomical ones.                                                                                                                        |

Searching and inserting links to other notes:

| Key binding | Description                                                                                                                                                                                                                   |
| ----------- | -----------                                                                                                                                                                                                                   |
| `C-c i`     | `helm-phi-find`. Search by note title or id. All these commands may be used to execute different actions (try `C-z`): open the note, insert wikilink to it, insert title and wikilink, insert title and wikikink and assign the note to the current project. |
| `C-c f f`   | `helm-ag-phi-find`. Search the contents of the repository with `helm-ag`. You may configure it for different engines (such as `ripgrep`).                                                                                     |
| `C-c f b`   | `helm-ag-phi-find-backlinks`. Search the notes for links poiting to the current one.                                                                                                                                          |
| `C-c f t`   | `helm-ag-phi-find-like-tags`. Search notes with similar tags.                                                                                                                                                                             |


## PHI notes anatomy

### Attribute fields

| Key       | Description                                                                                                   |
| --------- | -----------                                                                                                   |
| `title`   | The note title.                                                                                               |
| `id`      | The unique numerical identification for the note (almost aways preceded by φ).                                |
| `citekey` | A BibTeX key for the bibliographical reference associated with the note.                                      |
| `loc`     | The location (usually a page interval) in the associated bibliographical reference.                           |
| `tags`    | A sequence of hash tags separated by spaces.                                                                  |
| `proj`    | A sequence of wikilinks to the project(s) the note is assigned to.                                            |
| `origin`  | A callback URL to open the source for the note, when it is created and maintained in an external application. |




