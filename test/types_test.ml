type color =
  | Red
  | Green
  | Blue

type person = {
  name : string;
  age : int;
  favorite_colors : color list;
}