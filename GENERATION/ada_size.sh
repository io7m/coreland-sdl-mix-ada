#!/bin/sh

if [ $# -ne 2 ]
then
  echo "usage: typemap generics" 1>&2
  exit 111
fi

map="$1"
gen="$2"

(cat <<EOF
-- auto generated, do not edit

with Ada.Text_IO;
with Ada.Command_Line;

with SDL;
EOF
) || exit 112

for line in `./packages.sh ${map} || exit 112`
do
  echo "with $line;" || exit 112
done

(cat <<EOF

procedure Ada_Size is
  package IO renames Ada.Text_IO;
  package Command_Line renames Ada.Command_Line;

EOF
) || exit 112

./types-ada-size.lua "${map}" "${gen}" || exit 112

(cat <<EOF
  procedure Find (Name : String) is
  begin
    for Index in Types'Range loop
      if Types (Index).Name.all = Name then
        IO.Put_Line (Natural'Image (Types (Index).Size));
        return;
      end if;
    end loop;
    raise Program_Error with "fatal: unknown ada type";
  end Find;

begin
  if Command_Line.Argument_Count /= 1 then
    raise Program_Error with "fatal: incorrect number of args";
  end if;
  Find (Command_Line.Argument (1));
end Ada_Size;
EOF
) || exit 112
