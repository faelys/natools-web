with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Forms is

   P : constant array (0 .. 1) of Natural :=
     (3, 4);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (14, 15);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (13, 11);

   G : constant array (0 .. 18) of Unsigned_8 :=
     (0, 2, 0, 0, 0, 1, 0, 8, 0, 2, 0, 0, 3, 4, 0, 0, 7, 5, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 19;
         F2 := (F2 + Natural (T2 (K)) * J) mod 19;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 9;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Forms;
