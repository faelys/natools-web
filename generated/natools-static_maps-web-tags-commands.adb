with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Tags.Commands is

   P : constant array (0 .. 1) of Natural :=
     (4, 11);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (29, 1);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (5, 3);

   G : constant array (0 .. 34) of Unsigned_8 :=
     (9, 0, 0, 0, 0, 9, 0, 0, 0, 0, 16, 0, 0, 0, 0, 4, 0, 6, 8, 12, 11, 2,
      0, 3, 12, 4, 0, 14, 0, 0, 6, 0, 13, 4, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 35;
         F2 := (F2 + Natural (T2 (K)) * J) mod 35;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 17;
   end Hash;

end Natools.Static_Maps.Web.Tags.Commands;
