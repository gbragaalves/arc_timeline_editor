# Arc Timeline Builder

A [Shiny](https://shiny.posit.co/) web app for manually constructing and editing [Arc App](https://www.bigpaua.com/arcapp) timeline data — visits, trips, and locomotion samples.

## Em português: novidades e rotina

Resumo das funções adicionadas em outubro de 2026 (o resto do README segue em inglês; as âncoras usadas pela ajuda do app ficam iguais).

### Revisão em lote (Editar → Revisar em lote)

- **Editar a rota arrastando (P).** Com o botão 📍 ligado, arraste a linha proposta para criar um pino de passagem; arraste o início ou o fim (por exemplo, da rua para a saída do prédio — o horário da viagem não muda) e os pinos. A rota só é recalculada ao soltar; um clique simples não faz nada. Clique direito ou duplo num pino o remove. **Ctrl+Z** desfaz (pinos, regras, trechos, pontas e exclusões compartilham um histórico por viagem, salvo em `scratch/review_state.rds`).
  - **O pino edita só o trecho em volta dele, até onde a rota volta sozinha para a base.** A base continua sendo o casamento (`/match`). Do pino para a frente, a rota vai até um ponto distante (o próximo pino, o fim da rota ou 5 km adiante na base) e o trecho trocado termina onde ela se junta à base e segue nela por pelo menos ~150 m (a até 5 m, no mesmo sentido); para trás, o mesmo ao contrário, a partir do pino anterior, do início ou de 5 km antes. Assim, um pino na pista central de uma avenida com pista lateral segue a central até a rota voltar naturalmente para a base, sem ida e volta. Sem junção antes do ponto distante, o trecho vai até ele (até o próximo pino: um trecho só pelos dois). A tolerância é de 5 m porque pistas paralelas podem estar a 7–14 m uma da outra (Avenida Brasil, pista central e lateral). O ⚠ só aparece se o trecho novo for mais de 1,5× o trecho trocado **e** tiver retorno ou ida e volta; um desvio longo mas reto não leva ⚠. Mover o início ou o fim troca o trecho até onde a rota encontra a base; se o OSRM falhar, vale uma linha reta até 300 m de cada lado. Remover o pino devolve a base; um novo laço refaz a base e reaplica os pinos.
  - **Clique no pino (viagens no perfil Oficial):** "Contramão" (Não · Ruas menores · Todas) e "Conversões proibidas" (Respeitar · Ignorar). O padrão é contramão só em ruas menores, respeitando as conversões; "Ignorar" usa o servidor `official-noturn`. Mudar uma regra refaz só o trecho daquele pino.
  - **Clique direito num trecho junto a um pino:** "Rota (OSRM)", "Trajeto gravado" (as amostras entre os dois pontos, simplificadas) ou "Linha reta", para túneis, obras e o que o roteador não faz. Os trechos livres aparecem tracejados. Carro, ônibus e táxi têm só esse menu.
- **Período rápido (passo 1, Buscar).** Ao mudar a data inicial, a final vai para o mesmo dia (uma escolha = um dia; o calendário da data final abre no mês da inicial); uma data final antes da inicial leva a inicial junto. ◀ ▶ (ou Alt+← / Alt+→, fora de campos de texto) andam um dia, ou o período inteiro pelo seu tamanho (uma semana anda uma semana), e já buscam. **Tudo do período** marca todos os tipos de viagem, desmarca "Só sinalizadas", ordena por data e busca; "Buscar viagens" continua para filtros próprios. Os tipos escolhidos e "Só sinalizadas" ficam guardados no navegador. A regra da data inicial → final vale também para Visita, Rota manual, Importar, Editar amostras e as datas das janelas de rota OSRM e de edição de item (as horas não mudam).
- **Estação de início/fim das viagens de trilho.** O 📍 ao lado de Início e de Fim mostra no mapa todas as estações (metrô, trem, VLT; nomes a partir do zoom 13); um clique numa estação, ou a até cerca de 300 m dela (vale a mais perto), escolhe essa estação e refaz a proposta na hora, como se fosse escolhida na lista; Esc ou o 📍 de novo cancelam. A estação escolhida aparece destacada no mapa. As listas de Início e Fim têm busca em todas as estações de todos os sistemas: "ponto gravado", depois as mais próximas (com a distância) e as demais em ordem alfabética, com o sistema e a linha ("Maracanã — Metrô L2", "Maracanã — SuperVia"). Vale também para as divisões Jaé de trilho. O sistema e as linhas vêm do `reference/rails.gpkg` (operadora/rede da estação e as relações de linha do OSM).
- **Viagens de trilho sempre corrigíveis.** As linhas de Início e Fim (listas e 📍) aparecem em toda viagem de metrô, trem e VLT, mesmo sem proposta; com as duas estações escolhidas, a proposta é refeita a partir delas. Um toque no Jaé até 15 min antes do início (ou durante a viagem) a até ~300 m de uma estação do sistema da viagem define o Início ("· Jaé" na lista) e a partida = toque + 60 s, mesmo antes do início calculado; um toque longe de estação segue a regra da janela de 3 min. A escolha automática nunca põe a mesma estação nas duas pontas. Quando o Google não dá trecho de trilho (sugere caminhar numa parada, erro, tempo esgotado), o trajeto segue os trilhos do OSM entre as estações, pelas linhas do sistema da viagem (troca de linha só numa estação do mesmo sistema; nunca entre sistemas), com partida no toque Jaé + 60 s ou no início da viagem, chegada na última amostra perto da estação de Fim (ou no fim da viagem) e horários pela distância; o painel mostra "Fonte: trilhos OSM". As estações incluem as paradas de bonde/VLT (`railway=tram_stop`) e os trilhos guardam as linhas do OSM que passam por eles (refaça a referência de trilhos para usar).
- **Trilhos pelas relações de linha do OSM, com baldeações.** Cada trecho de trilho (uma linha, da estação X à Y) segue a relação de rota PTv2 do OSM daquela linha no sentido da viagem (a que tem X antes de Y), cortada entre as paradas de X e Y; sem relação com sentido, o grafo de trilhos respeitando `oneway` / `railway:preferred_direction`. Assim o VLT segue o binário (Rua da Gamboa num sentido, Rua Santo Cristo no outro). A geometria do Google não é mais usada: ele só sugere estações, linhas, baldeações e durações. Entre Início e Fim ficam as linhas de **Baldeação** (zero ou mais, com lista com busca, 📍, + e ×), preenchidas com as baldeações do Google quando ele dá uma rota; ao editar, o plano segue as suas estações. Uma baldeação entre duas estações diferentes do mesmo sistema a até ~400 m (Carlos Pellegrini ↔ 9 de Julio) vira uma caminhada curta (3 min por 250 m entre as estações) e uma trilha a pé no GPX; nunca entre sistemas. Se Início, Fim ou baldeações escolhidos por você (ou pelo Jaé) não batem com a rota do Google, o plano sai das relações do OSM com horários pela distância entre o início e o fim da viagem; se batem, valem as durações do Google. O painel mostra a fonte: "Fonte: Google" ou "Fonte: trilhos OSM". A referência de trilhos guarda as relações (id, linha, sentido, paradas em ordem, vias) — cobre Rio (metrô, SuperVia, VLT, bonde) e o Subte de Buenos Aires; refaça a referência para usar.
- **O GPX cobre a viagem original inteira.** O Arc só substitui o intervalo que o GPX cobre; por isso, ao enviar ou baixar (lote do Revisar em lote e o GPX de Editar amostras), as trilhas de cada viagem aceita cobrem exatamente o início e o fim do item original. Antes da proposta, se as amostras gravadas no intervalo ficam a até ~300 m do início (estação ou ponto), entra uma espera parada ali; se se afastam, uma caminhada com as próprias amostras (sem OSRM), terminando no início da proposta; o mesmo, espelhado, depois do fim. Intervalos de menos de 5 s só esticam o trecho vizinho. Uma proposta que passa do item (horário de estação extrapolado) é ajustada a ele, exceto a partida pelo toque no Jaé. No Revisar em lote, uma lacuna de mais de 60 s aparece antes de Aceitar ("Proposta cobre 18:27–18:36 de 18:15–18:36; 12 min antes: [espera em Praça Onze]"), com a escolha entre espera, caminhada gravada e estender o trecho principal até o horário original.
- **Laço de exclusão (L).** Com o botão ➰ (ou a tecla L), o modo laço fica ligado até você clicar de novo ou apertar Esc: cada arrasto no mapa desenha um laço, o mapa não se move (a roda do mouse continua dando zoom) e o botão fica laranja. Shift + arrastar desenha um laço mesmo com o modo desligado. Um arrasto que começa num pino ou na linha da rota em edição nunca é laço. Circule amostras com tremor do GPS (por exemplo, parado num sinal): elas saem da entrada do `/match`, mas continuam contando no tempo do trajeto. Ficam cinza com contorno; "Limpar exclusões" desfaz tudo.
- **Correções automáticas do casamento.** Esporas de ida e volta de até 150 m (o tremor parado num cruzamento que faz a rota entrar numa transversal e voltar) são cortadas do casamento; os blocos de 100 pontos do `/match` agora se sobrepõem em 20 pontos e cada um usa só o seu miolo (antes, o fim de um bloco às vezes grudava numa via paralela). O agrupamento de amostras paradas existe (`REVIEW_THRESHOLDS$match_fixes$collapse_stops`), mas fica desligado: no inventário de setembro de 2026 ele marcou mais viagens.
- **Notas Jaé e opções do Enviar.** Ao lado de "Enviar GPX em lote ao iCloud": "Adicionar notas Jaé" e "Nota da espera". A perna de ônibus recebe uma nota com dados da SMTR (linha e destino, carro e placa, consórcio e operadora, carroceria/chassi/ano, horários da viagem e embarques Jaé + RioCard), consultados no BigQuery com limite de custo e guardados em `reference/smtr_note_cache.rds`; a espera recebe o nome do ponto (GTFS da SMTR, depois Google Places), que pode ser editado antes de aceitar. As notas vão como um pacote só de notas para `Import/` no iCloud: no Arc, importe primeiro o GPX e depois a pasta Import.

### Editar amostras

- Pinos também por arrasto da linha (P), "Excluir do ajuste" para as amostras selecionadas no laço (o botão ➰ Laço, ou L, funciona como na revisão em lote: fica ligado até Esc; Shift + arrastar também seleciona) (elas não viram pontos de passagem, mas são posicionadas na rota) e Ctrl+Z. Pinos e exclusões valem no próximo "Ajustar".

### Camadas do mapa (controle de camadas, desligadas por padrão)

- **Infra de ônibus:** busways, `service=bus`, vias com `bus`/`psv` = yes|designated, faixas exclusivas (`busway:*=lane`, `lanes:bus`, `lanes:psv`), com setas nas mãos únicas; em cinza tracejado, vias `access=no|private` próximas sem permissão para ônibus. Clique numa via para ver as tags e o link para o OSM. Gerada de `~/osrm-data/clipped-custom.osm.pbf` por `tools/refresh_bus_infra.py` (manual) e refeita pela atualização noturna.
- **Rede do roteador:** as tiles vetoriais do OSRM (`/tile/v1`) do perfil escolhido na viagem (Carro / Oficial / Ônibus), a partir do zoom 14, coloridas pela velocidade e com setas de sentido.

### Atualização noturna do OSM (`~/osrm-data/nightly_refresh.sh`, cron às 04:00)

- Semanal: baixa a Geofabrik, recorta e monta a base (`clipped.osm.pbf` + `base_timestamp`) quando a última tem 7 dias (`refresh.conf`) e há arquivos novos. Diária: aplica **todos** os meus changesets desde a base − 2 dias sobre a base intocada e refaz só os perfis afetados (tags que não afetam roteamento não refazem nada). `.force_next` força uma atualização completa.
- **Toda reconstrução é uma janela de manutenção** (`refresh_osm.sh`: completa, `--profiles`, `--overrides-only`, `--speeds`, `--rollback` e a noturna): grava `~/osrm-data/MAINTENANCE` (início e motivo), para **todos** os servidores OSRM, reconstrói (vários ao mesmo tempo só se a RAM livre comportar o pico; senão um por vez), sobe todos, roda os testes de fumaça e apaga o arquivo. Enquanto ele existe, a bolinha do OSRM fica azul ("Roteamento em manutenção desde hh:mm (motivo)", conferida a cada 30 s) e as ações que dependem do OSRM mostram um aviso em vez de erro.
- Testes de fumaça depois de cada reconstrução (`smoke_tests/`): servidor fora do ar ou rota sem resultado volta ao dataset anterior (FAILED); rota que muda mais de 20% gera WARN.
- O perfil `official` trata `highway=busway` como liberado para ônibus/psv (a menos que `bus=no`/`psv=no`) e permite contramão em todas as vias menos rotatórias, motorway e trunk. O sentido contrário leva a classe `wwminor` (vias menores) ou `wwmajor` (primary, secondary e seus links); o app pede `exclude=wwmajor` por padrão, o que reproduz o comportamento anterior. `official-noturn` (porta 5006) é o mesmo perfil ignorando conversões proibidas.
- Status em `~/osrm-data/last_refresh.json`; logs em `~/osrm-data/logs/` (últimos 14).

### Limpeza do iCloud (`tools/icloud_cleanup.R`, cron às 03:30)

- Só mexe em `GPX/` e `Import/` (qualquer outro caminho é recusado no código). Um pacote de notas em `Import/` é apagado quando todas as notas já estão no backup do Arc; GPX enviados pelo app (registrados em `exports/gpx_send_log.csv`, cópia em `exports/sent_gpx/`) são apagados depois de `GPX_RETENTION_DAYS` — desligado por padrão (`GPX_CLEANUP_ENABLED`). `--dry-run` mostra o que seria apagado.

### Bolinhas de status (barra superior)

- **OSRM:** verde = servidores no ar; a dica mostra "Base OSM: dd/mm/aaaa · minhas edições até dd/mm hh:mm"; **laranja** = a última atualização noturna falhou (motivo na dica).
- **Google:** chave da API Directions válida.
- **iCloud:** rclone configurado; **laranja** = "Reconecte: rclone reconnect icloud:" ou "Pacote de notas aguardando Restore desde dd/mm" (há mais de 2 dias).

### Rotina: revisar um mês

1. **Buscar.** Editar → Revisar em lote → 1 · Buscar: escolha os tipos e o mês (o cache em `scratch/` deixa a segunda vez rápida).
2. **Aceitar em lote.** "Aceitar em lote" pega as propostas seguras (0,9–1,2× o comprimento gravado e sem sair das vias no novo casamento).
3. **Revisar o resto, da pior para a melhor** (ordenar por gravidade): desvios possíveis → "Nova rota (R)"; trechos com tremor → laço (L); rota errada → editar arrastando (P) e soltar; fragmentos → apagar ou marcar como bogus no próprio Arc.
4. **Viagens com Jaé:** "Dividir" (caminhada → espera → ônibus/metrô), clicar onde desceu, conferir o nome do ponto e aceitar.
5. **Enviar.** 3 · Exportar → "Enviar GPX em lote ao iCloud" com "Adicionar notas Jaé"; no Arc: importar o GPX, depois a pasta Import.
6. **Conferir no dia seguinte** as bolinhas: iCloud laranja = pacote de notas ainda não importado.

## Why?

Arc is great at automatic recording, but sometimes you need to fix or build timeline data manually:

- **Snap-to-road** messy GPS paths using a local [OSRM](http://project-osrm.org/) server
- **Fix metro/subway routes** where GPS was poor underground
- **Fill data gaps** where Arc lost recording
- **Import flight tracks** from FlightRadar24 KML files
- **Build timeline entries from scratch** when the phone was off
- **Edit existing samples** — drag markers, ignore bad points, re-snap entire paths

## Arc Editor migration

This tool originally exported in the LocoKit format used by Arc Timeline. It now targets **Arc Editor** (the ground-up rebuild of Arc Timeline) and generates data in the **LocoKit2 JSON format** (schema 2.2.0).

> The export format matches Arc Editor's LocoKit2 JSON format (schema 2.2.0) and imports successfully via Restore from Backup.

## Features

### Editing modes

| Mode | Description |
|------|-------------|
| **Route** | Click waypoints, select a transport profile (car/official/foot/bike/bus via OSRM, or metro/train/tram via Google Maps), calculate a snap-to-road route. Can start from a Jaé fare card boarding, see [Jaé boardings](#jaé-boardings-optional). |
| **Visit** | Mark a point, name it, set entry/exit times. Frequent places auto-fill coordinates. |
| **Manual Route** | Click points for a polyline, set times, pick an activity type. |
| **Import File** | Import GeoJSON, GPX, KML, or GPKG. Special handling for FlightRadar24 KML. |
| **Edit Samples** | Load existing samples from weekly `.json.gz` backups (optionally fetched from iCloud). Drag, ignore, lasso-select, snap-to-road, export as GPX or JSON. |
| **Edit Route** | Select a trip, edit its waypoints (drag, delete, insert), apply changes preserving average speed. |
| **Batch review** | Find trips whose recorded path doesn't follow the rails or roads, review a proposed clean path for each, and export the accepted ones as one GPX. See [Batch review](#batch-review). |

### Activity types

The tool supports all [LocoKit2 activity types](https://github.com/sobri909/LocoKit2/blob/main/Sources/LocoKit2/ActivityTypes/ActivityType.swift):

| Code | Type | Code | Type | Code | Type |
|------|------|------|------|------|------|
| 1 | stationary | 24 | tram | 50 | skateboarding |
| 2 | walking | 25 | tractor | 51 | inline skating |
| 3 | running | 26 | tuk tuk | 52 | snowboarding |
| 4 | cycling | 27 | songthaew | 53 | skiing |
| 5 | car | 28 | scooter | 54 | horseback |
| 6 | airplane | 29 | metro | 55 | swimming |
| 20 | train | 30 | cable car | 56 | golf |
| 21 | bus | 31 | funicular | 57 | wheelchair |
| 22 | motorcycle | 32 | chairlift | 58 | rowing |
| 23 | boat | 33 | ski lift | 59 | kayaking |
| | | 34 | taxi | 60 | surfing |
| | | 35 | hot air balloon | 61 | hiking |

### Export format

Generates a zip in the LocoKit2 format:

```
metadata.json               # Export metadata (schema 2.2.0)
items/2026-03.json          # Timeline items grouped by month
samples/2026-W12.json.gz    # Samples grouped by ISO week (gzipped)
places/0.json               # Places referenced by visits
```

In **Edit Samples** mode, the corrected route is exported first as GPX (**Send GPX to iCloud** / **Export GPX**), which is the way to replace a bad route (see [Replacing an existing route](#replacing-an-existing-route)). Below it, under **Arc JSON package**, a mode selector controls how **Export (zip)** / **Send to iCloud (Import)** write the route:

| Mode | Item | Use |
|------|------|-----|
| **Add** (default) | New ID, enabled | Corrected route added alongside the original; mainly for filling empty periods |
| **Disabled item** | New ID, item and samples `disabled` | Imports fine but stays hidden: Arc has no UI to enable it |
| **Reuse original item ID** | Original ID | Not working yet: needs upsert support in the importer |

Exported samples always get new IDs and the original trip's activity type, and the item gets real distance and speed. The GPX contains the same samples as the zip, with the activity type in `<trk><type>`.

### Importing into Arc Editor

1. In the Shiny app, open the **Export** tab and click **Download Arc package (zip)** (for Edit Samples: **Edit → Edit samples → 3 · Export → Arc JSON package → Export (zip)**)
2. The zip is named `Import.zip` — extract it to get the `Import` folder
3. Upload the `Import` folder to **iCloud Drive**, then move it inside the **Arc Editor** folder
4. In Arc Editor on your device: **Menu > Backup & Restore > Restore from Backup**
5. Select the `Import` folder and tap **Open**
6. After import, Arc will run an automatic incremental backup that includes the new data

With [iCloud sync](#icloud-sync-optional-via-rclone) set up, **Send Arc package to iCloud** / **Send to iCloud (Import)** replace steps 2–3: the package is written straight to `Arc Timeline/Import`.

> **Important — existing data is never modified by import.** See [How the LocoKit2 Import Works](#how-the-locokit2-import-works) below for details.

### Other features

- **Overlap detection** — prevents overlapping timeline items
- **Auto-suggest times** — next item starts 1 minute after the previous one
- **Multi-timezone** — detects timezone from coordinates
- **GPX export** — the day's trips (Export tab: **Send timeline GPX to iCloud** / **Download timeline GPX**), one track per trip with its activity type and altitude; all samples including visits: ⚙ menu
- **OpenFreeMap base map** — vector base layer by default (falls back to OSM if it can't load); OSM, CartoDB, Satellite and Topo in the layers control
- **No page scrolling** — the layout fits the browser window (1366×768 and up): Create / Edit / Export tabs on the left, the map in the middle, the day timeline on the right; both sidebars can be collapsed
- **Portuguese / English** — see [Language](#language)

## How the LocoKit2 Import Works

These findings come from investigating the [LocoKit2 source code](https://github.com/sobri909/LocoKit2) (`ImportManager.swift`, `SampleImportProcessor.swift`, `ExportMetadata.swift`):

- The **ImportManager** uses `INSERT OR IGNORE` (GRDB's `.ignore` conflict resolution) for **all data types**: places, timeline items (base, visit, trip), and samples
- This means import is **strictly additive** — records with IDs that already exist in the database are silently skipped, never updated
- The `lastSaved` field is used by the **ExportManager** to determine which weekly/monthly buckets to re-export during incremental backups. This comparison happens on the **export** side, not during import
- **Samples** in LocoKit2 use `disabled` instead of `deleted` (which existed in the original LocoKit). The `deleted` field is not part of the `LocomotionSample` model
- **Timeline items** (`TimelineItemBase`) still have a `deleted` field for soft-delete, but since existing records are skipped during import, setting `deleted: true` on an item that already exists has no effect
- The `ExportType` enum in `ExportMetadata.swift` accepts only `"full"` and `"incremental"` — any other value causes a decode error

### Implication for route correction

It is **not possible** to replace or delete existing data via import. When importing a corrected route:

- The corrected route is added as a **new item** alongside the original
- The original item and its samples remain unchanged in the database
- There is no supported mechanism to remove or update existing items/samples through the import pipeline

This is a fundamental constraint of the LocoKit2 ImportManager design (`INSERT OR IGNORE`), not a bug in this tool.

## Known Limitations

- **Cannot replace existing items/samples via import** — the LocoKit2 ImportManager uses `INSERT OR IGNORE`, so records with existing IDs are always skipped
- **Old route remains after importing a correction** — the corrected route is added as a new separate item; the original stays in the database. Use the GPX workflow below instead
- **`deleted: true` on items has no effect during import** — the item already exists with the same ID, so the import record is ignored entirely
- **`deleted` field on samples is not recognized by LocoKit2** — the `LocomotionSample` model uses `disabled` instead (inherited from the LocoKit1 -> LocoKit2 migration)
- **Marking items as "bogus" in Arc Editor** changes the activity type but does not remove the item or its samples from the timeline

### Replacing an existing route

Verified on device (Arc Timeline 4): import a **GPX** of the corrected route with Arc's own **Import GPX File**. Arc reads the activity type from `<trk><type>` (e.g. Metro), shows the local time correctly, warns that the GPX overlaps existing timeline data, and then disables and hides the overlapping data, replacing the bad trip. Internally this is LocoKit2's `enableItem(itemId:)` (`TimelineProcessor+EnabledState.swift`): items that overlap the new one are disabled along with their samples, and partial overlaps are split.

1. In Edit Samples mode, load the samples, keep only the boarding and alighting points (Ignore/Discard the rest), and **Snap**. Metro/Train/Tram use Google transit directions (origin to destination only); road modes use OSRM.
2. Click **Send GPX to iCloud** (or **Export GPX** and copy the file to iCloud yourself).
3. In Arc, use **Import GPX File** and pick the file from `Arc Timeline/GPX`.

The existing data is disabled and hidden, not deleted, and Arc can undo the import.

The same works for any trip built in this app, e.g. a flight imported from a FlightRadar24 KML over a badly recorded one: in the **Export** tab, **Send timeline GPX to iCloud** (or **Download timeline GPX**) writes `arc_timeline_<date>_<HHMM>.gpx` with one `<trk>` per trip of the day timeline (visits are skipped), each with its activity type in `<trk><type>` and altitude in `<ele>` when known. Arc imports each track as its own item.

The JSON **Disabled item** mode is not an alternative: the package imports fine (Arc logs the item and samples), but the item stays hidden and Arc has no UI to enable it.

### Batch review

**Edit → Batch review** fixes many trips at once. Distance and speed are poor signals for a bad recording; geometry isn't: a metro trip should follow the rails, a car trip the roads.

1. **Find**: pick the activities (metro, train, tram, car, taxi, bus) and a date range. Trips come from the backup's monthly `items/` files (tick *Fetch everything from iCloud first* to copy `samples/` and `items/` with rclone; nothing is written to iCloud). Each trip's recorded path is densified every 20 m and compared with its network:
   - **rails** for metro/train/tram, from `reference/rails.gpkg` (built from the OSM extract on first use, or via ⚙ → *Rebuild rail reference*; osmium when installed, otherwise GDAL's OSM driver);
   - **roads** for car/taxi/bus, via OSRM `/match` on the local server (official profile for car, car profile for taxi, bus profile for bus; see [OSRM](#osrm-optional)).

   A trip is flagged on `off_share` (share of the path farther than the tolerance), `longest_off_m` (longest stretch off the network), `max_offset_m` (rail), no road match at all, or a recorded path under 0.5 km (a *fragment*; round trips with a real path are not fragments). A GPX import can't remove data, so fragments get a *fragment* badge suggesting to delete them or mark them as bogus in Arc, and are never accepted in bulk. Road trips are judged on geometry only, with a 35 m tolerance for urban-canyon noise; OSRM's match confidence is shown but doesn't flag (it is often near 0 on dense recordings, as the minimum over 100-point chunks). All thresholds are in `REVIEW_THRESHOLDS` at the top of `R/utils_review.R`. Trips imported from GPX (source other than LocoKit2) show as *already fixed*. Results are cached per trip in `scratch/review_cache.rds`, so a second Find is instant. No Google calls happen here.
2. **Review**: the list scrolls on its own; click a row to see on the map the reference network, the recorded path coloured by offset (grey within the tolerance, red beyond) and the proposed clean path. **A** accepts, **S** skips, **R** re-routes; *Use the recorded path as is* marks the trip OK.
   - **Rail**: origin and destination are the first/last samples with accuracy ≤ 50 m (right-click a sample on the map: *Set as start / Set as end*). Each end snaps to the nearest station of the mode within 600 m (behind the first samples, ahead of the last ones; the selects offer the 3 nearest). Google transit routes station to station, with the original weekday and time of day. The clean path starts before the first sample by the along-rail distance at 35 km/h (at most 3 min) and ends likewise after the last; −1 min … +1 min nudge the start and end times. In between, the time follows Google's steps: the real duration is shared among the legs and transfers in proportion to Google's durations (a transfer is the walk, then a wait standing still at the next platform: the gap between Google's arrival and departure times), linear by distance within each; the panel shows the legs, e.g. *Linha 2 · 18 min → transfer Estácio 4 min → Linha 1 · 9 min* (Google's own times and the scale factor in the tooltip). When Google gives no step durations, times are linear by distance and the panel says so. A warning shows when Google's route itself strays from the rails.
   - **Rail legs from OSM route relations**: each leg (one line, station X to Y) follows the line's PTv2 route relation in the direction of travel (the one with X before Y), cut between X's and Y's stops; a relation without direction falls back to the rails graph honouring `oneway` / `railway:preferred_direction`. Google's polyline isn't used: Google only suggests stations, lines, transfers and step durations. **Transfer** rows (zero or more, searchable select, 📍, add / remove) sit between Start and End, prefilled from Google's transfers; once edited, the plan follows them. A transfer between two different stations of one system within ~400 m is a short walk (3 min per 250 m station to station) and a walking track in the GPX; never across systems. When the stations you (or Jaé) set disagree with Google's route, the plan comes from the OSM relations, timed by distance between the trip's start and end; when they agree, Google's step durations time it. The panel shows *Source: Google* or *Source: OSM rails*.
   - **Rail without a Google leg**: the Start/End rows (selects and 📍) always show, even without a proposal. A Jaé tap up to 15 min before the trip (or during it) within ~300 m of a station of the trip's system makes that station the Start ("· Jaé") and the departure tap + 60 s; otherwise the 3-minute gate rule applies. The automatic picks never put the same station at both ends. When Google returns no transit leg (walking for one stop, an error, a timeout), the leg follows the OSM rails (`reference/rails.gpkg`: tracks with the OSM lines that run on them) between the stations, on the lines of their system, changing line only at a station of that system; times are linear by distance from the departure (tap + 60 s, else the trip's start) to the arrival (the last sample near the End station, else the trip's end). The panel says *Source: OSM rails* and, without a route, why.
   - **Road**: the matched geometry, with the real sample times at the matched points (speed varies like the trip); gaps between matched pieces are routed with OSRM. Find builds this proposal for every flagged road trip (local OSRM only) and checks it: its length against the recorded one, and the proposal matched again, which should leave ~0 % off the roads. A proposal over 1.25× the recorded length gets an orange *detour?* badge, one under 0.9× a *skips part?* badge (it possibly skips part of the trip); **R** then routes through the recording too (OSRM `/route` via one sample every ~300 m, snapped in the travel direction, loops around badly snapped waypoints dropped) and both candidates are shown with their lengths to pick from. Otherwise **R** routes between the start and end samples. *Accept all* accepts, after a confirmation with the count, the flagged road trips whose proposal is 0.9–1.2× the recorded length and stays on the roads when matched again; fragments, detours, proposals under 0.9× and trips re-routed by hand are left out.
3. **Export**: **Send batch GPX to iCloud** / **Download batch GPX** write `arc_batch_<types>_<first>_<last>.gpx`, one `<trk>` per accepted trip. Arc replaces only the time range a GPX covers, so each accepted trip's tracks cover its original item exactly: a gap before the proposal becomes a stationary track at its start when the recorded samples there stay within ~300 m of it, else a walking track from those samples (no OSRM); likewise after its end; gaps under 5 s just stretch the neighbouring track; a proposal reaching outside the item is fitted into it (except a departure from a Jaé tap). A gap over 60 s shows in the detail before Accept, with a select: wait, recorded walk, or extend the main leg to the original time. The Edit Samples GPX (download and send) follows the same rule with the item's original samples. *Send only checked* exports only the ticked rows: Arc's import of a GPX with many tracks is not verified yet, so try 2–3 trips first. Accepted trips are saved in `scratch/review_state.rds`; reopening the mode offers *Resume previous review*.

## Requirements

### R packages

```r
install.packages(c(
  "shiny", "leaflet", "httr", "jsonlite", "sf", "dplyr",
  "lubridate", "uuid", "lutz", "xml2", "stringr", "purrr",
  "geosphere", "zip"
))
```

### OSRM (optional)

Local OSRM servers via Docker for snap-to-road (car, foot, bike, bus, official and official-noturn profiles):

| Profile | Port | OSRM profile |
|---------|------|--------------|
| Car | 5000 | driving |
| Foot | 5001 | foot |
| Bike | 5002 | bicycle |
| Bus | 5003 | driving |
| Official | 5005 | driving |
| Official, turn restrictions ignored | 5006 | driving |

*Official* is a car that may also use bus/psv roads: the stock car profile, plus busways, `service=bus` roads and any way or barrier where a `bus`/`psv` permission overrides the car's access (BRT corridors and terminals, for example). Batch review matches car trips with it (`ROAD_PROFILE_FOR_CAR`, default `"official"`, or `"car"` when the official server isn't enabled); taxi trips use car and bus trips use bus. A profile select next to **Re-route** matches the selected road trip again with Car, Official or Bus.

Official may also drive against one-way traffic, except on roundabouts, motorways and trunks; the reverse direction carries the class `wwminor` (minor streets) or `wwmajor` (primary, secondary and their links). The app asks the official servers with `exclude=wwmajor` by default (`OSRM_DEFAULT_EXCLUDE`), which keeps wrong-way to minor streets. *official-noturn* (`official-noturn.lua`) is the same profile without turn restrictions; Batch review uses it only for a pin whose "Restricted turns" rule is "Ignore".

While `~/osrm-data/refresh_osm.sh` rebuilds the data it writes `~/osrm-data/MAINTENANCE` (`OSRM_MAINTENANCE_FILE`) and stops every server; the OSRM status dot turns blue with the start time and the reason, is re-checked every 30 s, and OSRM-dependent actions show a notice instead of an error.

`OSRM_ENABLED_PROFILES` in `config_local.R` lists the servers in use (default `c("car", "foot", "bus", "official", "official-noturn")`; a profile without a server is ignored). The others are left out of the profile selects and the OSRM status dot, so a stopped server doesn't show as an error. Add `"bike"` back to use the bike server again. Cycling remains an activity type for manual routes and GPX export either way.

See the [OSRM backend guide](https://github.com/Project-OSRM/osrm-backend#using-docker) for setup.

### Google Maps API (optional)

For metro, train, and tram routes, set the `GOOGLE_MAPS_API_KEY` environment variable in your `.Renviron`:

```
GOOGLE_MAPS_API_KEY=your_key_here
```

The key needs the **Directions API** enabled in the [Google Cloud Console](https://console.cloud.google.com/apis/library/directions-backend.googleapis.com).

Google transit directions don't support waypoints, so transit routes always go from the first point to the last; intermediate points are ignored.

### iCloud sync (optional, via rclone)

Fetches the weekly sample backups and sends import packages / GPX files without moving files by hand. The iCloud controls only appear when rclone and the remote are available.

1. Install [rclone](https://rclone.org/install/) (a version with the `iclouddrive` backend).
2. Run `rclone config` and create a new remote named `icloud`, type `iclouddrive`, with your normal Apple ID password and the 2FA code.
3. Check it with `rclone lsd icloud:`.

The login creates a trust token that lasts about 30 days. When it expires, the app shows "iCloud authorization expired"; renew it in a terminal with `rclone reconnect icloud:`. With **Advanced Data Protection** on, **Access iCloud Data on the Web** must be enabled on the iPhone.

The app reads weekly backups from `Arc Timeline/Backup/<backup-id>/samples/`, picking the backup folder with the most recently modified file. Optional overrides in `config_local.R`:

```r
ICLOUD_REMOTE    <- "icloud"        # rclone remote name
ICLOUD_ARC_DIR   <- "Arc Timeline"  # Arc folder in iCloud Drive
ICLOUD_BACKUP_ID <- "<backup-id>"   # skip auto-detection
```

- **Fetch weeks from iCloud first** (Edit Samples) copies the needed weeks into `WEEKLY_BACKUP_DIR` before loading. Only changed files are downloaded. If it fails, the app loads what is already on disk.
- **Send GPX to iCloud** copies the GPX to `Arc Timeline/GPX/`.
- **Send to iCloud (Import)** / **Send Arc package to iCloud** **replace** the contents of `Arc Timeline/Import` (`rclone sync`), so leftovers from earlier imports are removed.

### Jaé boardings (optional)

For Rio de Janeiro's Jaé fare card. Set `JAE_CSV_PATH` in `config_local.R` to a CSV export of your fare history. The Route pane then lists the work date's boardings under **Jaé boardings**, and the map shows them as markers coloured by mode (bus, BRT, metro, VLT); a marker's popup has the time, line or station, fare type and fare.

**Start route here** puts the route's first point on the boarding and sets the profile (bus and BRT → bus, metro → metro, VLT → tram). When you calculate the route, the start time is the tap-in time, and the confirmed route is described by the boarding (`<line> · <direction> · veículo <id>` for buses, `Metrô · <station>`, `VLT · <stop>`). A boarding without coordinates shows "no location": after **Start route here**, click its location on the map to set the first point. Boardings already turned into a route in the current session are greyed out.

**Split at the boarding (Batch review).** Arc often starts a bus trip at the front door (and may call the bus ride "car"). A bus or car trip that contains a bus/BRT tap, or a metro or train trip that contains a Metrô gate tap, shows `Jaé: <time> · <line or station>` in the detail and **Split: walk → wait → bus** (or metro). The split replaces the trip with separate tracks, in time order and touching:

- the walk to the stop as recorded, up to the first sample within 40 m of the tap point before the tap (or the nearest one before it);
- a stationary wait at the stop until the tap;
- the ride. For a bus, click the map where you got off: the click snaps to the nearest recorded sample after the boarding along the recording, and the bus leg is matched on the bus profile. For rail, the leg is the rail fix; the wait runs until its departure.
- after alighting (bus): the recorded walk, a visit at the destination until the trip's end, or nothing (*leave as is*).

The map previews each segment in its colour. A tap up to 3 minutes after the rail leg's computed start moves the start to the tap plus `rail_gate_to_train_s` (60 s, in `REVIEW_THRESHOLDS`); a later one is ignored (probably another trip). A Metrô tap without coordinates stands at the departure station. **OSRM** routes the walks with OSRM foot instead, as a fallback.

The CSV is UTF-8, comma-separated, one row per boarding (tap-in only; there is no alighting data), with the columns `datetime_local`, `datetime_utc` (ISO, `Z`), `modo` (`Ônibus` | `BRT` | `Metrô` | `VLT`), `consorcio`, `linha_ou_estacao`, `descricao`, `sentido`, `id_veiculo`, `tipo_transacao`, `produto`, `meio_pagamento`, `valor`, `latitude`, `longitude`, `fonte_coordenada`, `usar_no_arc` (`sim` | `não`), `obs`. Only rows with `usar_no_arc` = `sim` are used, and dates are matched in `America/Sao_Paulo`. Without `JAE_CSV_PATH`, or if the file is missing, nothing is shown. Keep the CSV outside the repository: it holds your trips and locations.

## Setup

1. Clone this repo
2. Create `config_local.R` (not tracked) with your local settings:

```r
# Path to weekly sample backup files (.json.gz)
WEEKLY_BACKUP_DIR <- "path/to/weekly/samples"

# Frequently-used places (optional)
FREQUENT_PLACES <- list(
  "Home" = list(lat = -23.5505, lon = -46.6333),
  "Work" = list(lat = -23.5600, lon = -46.6500)
)

# Default UI language (optional): "pt" (default) or "en"
APP_LANGUAGE <- "pt"

# Jaé fare history CSV (optional): boardings listed in the Route pane
JAE_CSV_PATH <- "path/to/jae_history.csv"

# Stop names for split waits (optional): Google account for tools/refresh_gtfs_stops.R
GTFS_BQ_EMAIL <- "you@example.com"

# OSRM servers in use (optional; default shown). Add "bike" to use the bike server
OSRM_ENABLED_PROFILES <- c("car", "foot", "bus", "official", "official-noturn")

# Batch review (optional; defaults shown)
ITEMS_BACKUP_DIR    <- file.path(dirname(WEEKLY_BACKUP_DIR), "items")  # monthly items from the backup
OSM_PBF_FILE        <- "~/osrm-data/clipped.osm.pbf"                  # source of reference/rails.gpkg
ROAD_PROFILE_FOR_CAR <- "official"                                     # OSRM profile for car trips
RAIL_REFERENCE_FILE <- "reference/rails.gpkg"
REVIEW_SCRATCH_DIR  <- "scratch"                                     # review_cache.rds, review_state.rds
```

3. Run the app:

```r
shiny::runApp()
```

## Language

The interface is available in Brazilian Portuguese and English. Switch with the **PT | EN** toggle in the top bar: texts change in place, without reloading or losing what you were doing, and the browser remembers the choice. The default for a new browser is `APP_LANGUAGE` in `config_local.R` (`"pt"` if unset). Dates are shown as `dd/mm/yyyy` in Portuguese and `yyyy-mm-dd` in English. Arc's own feature names (Import GPX File, Restore from Backup) are kept as Arc shows them.

Translations live in `i18n/translations.json` (English key → `pt` / `en`). After adding or changing user-facing text, run `Rscript i18n/check_translations.R` from the project root: it fails if a key used in the code is missing, a key is unused, or placeholders like `%d` differ between languages.

## Project structure

```
app.R                          # Entry point
i18n/translations.json         # UI translations (PT/EN) + check_translations.R
R/
  utils_time.R                 # Time parsing, timezone detection, interval validation
  utils_geo.R                  # Geographic calculations, FR24 KML parsing, timed path helpers
  utils_osrm.R                # OSRM routing, map matching (/match), snap-to-road, route smoothing
  utils_google.R              # Google Maps Directions API (transit routes)
  utils_arc.R                 # LocoKit2 data model: samples, items, places, activity types
  utils_jae.R                 # Jaé fare card boardings (CSV): profile, label, day filter
  utils_weeks.R               # Weekly backup file I/O, overlap detection, sample loading
  utils_icloud.R              # iCloud Drive round-trip via rclone (fetch backups, send imports/GPX)
  utils_i18n.R                # UI language (PT/EN): translation lookup, date formats
  utils_review.R              # Batch review: items, rail reference, conformity metrics, fixes
  utils_stops.R               # Stop names for split waits: SMTR GTFS, Google Places, generic
  utils_smtr.R                # SMTR bus note for Jaé bus legs (BigQuery, cached per date and vehicle)
  utils_icloud_cleanup.R      # Cleanup of GPX/ and Import/ in iCloud, GPX send log, status for the iCloud dot
  utils_layers.R              # Map overlays: bus infrastructure (by bounds), OSRM router network tiles
  ui_route_edit_js.R          # Client: route editing by dragging, sample lasso, Ctrl+Z, overlays
  server_review.R             # Batch review server (Edit → Batch review)
  export_arc.R                # LocoKit2 JSON export (zip package generation)
  ui.R                        # Shiny UI layout and controls
  server.R                    # Shiny server logic, editing modes, import/export handlers
reference/rails.gpkg           # Rails + stations from OSM (built locally, not tracked)
reference/gtfs_stops_rio.csv   # SMTR GTFS stops, latest feed (tools/refresh_gtfs_stops.R; not tracked)
reference/places_cache.rds     # Google Places stop names by rounded coordinates (not tracked)
tools/refresh_gtfs_stops.R     # Refreshes reference/gtfs_stops_rio.csv from BigQuery (bigrquery)
tools/refresh_bus_infra.py     # Builds reference/bus_infra.geojson (bus infrastructure layer) from the routing extract
tools/refresh_rails.R          # Rebuilds reference/rails.gpkg from an extract (nightly refresh)
tools/icloud_cleanup.R         # Daily iCloud cleanup (cron 03:30, --dry-run)
tests/test_review_unit.R       # Unit tests on synthetic data (Rscript tests/test_review_unit.R)
scratch/                       # Batch review cache and saved review (not tracked)
```

## License

[MIT](LICENSE)
