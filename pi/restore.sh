#!/usr/bin/env bash
# Restore pi config on a new machine. Run: bash pi/restore.sh
set -euo pipefail
cd "$(dirname "$0")"
A="$HOME/.pi/agent"
mkdir -p "$A/extensions" "$A/themes" "$HOME/.pi/rules" "$A/skills" "$HOME/.agents/skills"
cp settings.json models.json "$A/"
cp extensions/*.ts "$A/extensions/"
cp themes/*.json "$A/themes/"
cp rules/*.md "$HOME/.pi/rules/"
cp -R skills/* "$HOME/.agents/skills/"
ln -sfn "../../../.agents/skills/implement" "$A/skills/implement"
for s in design-an-interface edit-article improve-codebase-architecture teach; do
  ln -sfn "../../../.agents/skills/$s" "$A/skills/$s"
done
echo "diff restored: add creds with 'pi auth' then launch pi to reinstall packages"
