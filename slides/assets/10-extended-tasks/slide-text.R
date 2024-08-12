sprintf(r"(## One User

```{=html}
{{< include assets/10-extended-tasks/one_user/%02d_one_user.excalidraw.svg >}}
```
)", 1:10) |> clipr::write_clip()


sprintf(r"(
## Two Users

::: {.easy-columns}
::: {.col .text-center}
Me

```{=html}
{{< include assets/10-extended-tasks/one_user/%02d_one_user.excalidraw.svg >}}
```
:::

::: {.col .text-center}
You

```{=html}
{{< include assets/10-extended-tasks/one_user/%02d_one_user.excalidraw.svg >}}
```
:::
:::
)", c(2:10, rep(10, 8)), c(rep(2, 8), 2:10)) |> 
  gsub("06_one_user.excalidraw.svg", "06_one_user.svg", x = _, fixed = TRUE) |> 
  clipr::write_clip()

sprintf(r"(
## Shiny Async: Two Users

::: {.easy-columns}
::: {.col .text-center}
Me

```{=html}
{{< include assets/10-extended-tasks/one_user/%02d_one_user.excalidraw.svg >}}
```
:::

::: {.col .text-center}
You

```{=html}
{{< include assets/10-extended-tasks/one_user/%02d_one_user.excalidraw.svg >}}
```
:::
:::
)", 2:10, 2:10) |> 
  gsub("06_one_user.excalidraw.svg", "06_one_user.svg", x = _, fixed = TRUE) |> 
  clipr::write_clip()


sprintf(r"(## Extended Tasks

```{=html}
{{< include assets/10-extended-tasks/%02d_extended_task.excalidraw.svg >}}
```
)", 1:11) |> clipr::write_clip()
