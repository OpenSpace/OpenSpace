# Writing a JSON Schema

Schemas should preferably be written directly as JSON files to ensure correct syntax highlighting, formatting, and validation.

LLMs can be useful for generating an initial draft, but the result must always be reviewed and aligned with existing conventions. When using an LLM, provide:

  - One or two examples of existing schemas, `errorlogtopic.json` and `eventtopic.json` are two good candidates since they demonstrate most patterns used.
  - Relevant engine source code for the topic

## Validating Your Schema

You can validate your schema and inspect the generated TypeScript types using the [JSON Schema to TypeScript Browser](https://borischerny.com/json-schema-to-typescript-browser/) Browser

Paste your schema into the tool to:

  - Verify that it produces the expected TypeScript types
  - Catch structural issues early
  - Ensure `topicPayload` and `data` map correctly to the API

## General Conventions

  - Use 2-space indentation for all JSON files

## Required Structure

  - `$defs`
    - Must be declared at the top of the schema
    - Contains reusable type definitions
  - `title`
    - Must end with `Topic` (e.g., `AuthorizationTopic`)
  - `topicId`
    - Must match the identifier defined in `server.cpp` for the corresponding topic
    - Must be defined using `const`

## Top-Level Contract (Strict)

All schemas **must** define exactly the following top-level properties. This structure defines the contract between the API and the client: `topicPayload` is the input, and `data` is the streamed output.

  - `topicId`
  - `topicPayload`
  - `data`

No additional top-level properties are allowed. This must be enforced using:

```json
"additionalProperties": false
```

at the root level.

### topicId

`topicId` is the unique identifier of the topic. It must match the identifier defined in `server.cpp`. This identifier is used by the API to select which topic to start.

### topicPayload

`topicPayload` defines the structure of the input data required to start the topic. This corresponds to the payload passed via the API, for example:

```ts
api.startTopic("authorize", {
  // Must conform to topicPayload schema
  password: 'pw'
});
```

### data

`data` defines the structure of the data emitted by the topic. This corresponds to the data accessed via the API's async iterator, for example:

```ts
for await (const _data_ of authorizeTopic.iterator()) {
    // _data_ has the shape of `data`
}

```

## Validation Rules

  - `additionalProperties`
    - Must be set to `false` on all objects unless keys are truly dynamic (e.g., scene node names, mission identifiers)
    - When keys are runtime strings, use `additionalProperties: { "$ref": "#/$defs/SomeType" }`
    - When applicable, this applies to all objects in the schema
  - Enums vs Union (`anyOf`)
    - Use `enum` for simple sets of constant values (e.g., strings or numbers)
    - Avoid `anyOf` when `enum` is sufficient
  - `anyOf` vs `oneOf`
    - Prefer `anyOf` over `oneOf` for performance and predictability
    - Only use `oneOf` when strict mutual exclusivity is required
    - See [documentation](https://json-schema.org/blog/posts/applicability-json-schema-fundamentals-part-1#putting-everything-together-avoiding-oneof-pitfalls)
  - **Discriminated Union Pattern**
    - Use `anyOf` (NOT `oneOf`) for multi-variant payloads where a `type` or `event` const field distinguishes the variants
    - Each variant must define its own const value for the discriminator field
    - This pattern produces clean TypeScript unions, for example:

```json
{
  "topicPayload": {
    "type": "object",
    "anyOf": [
      {
        "type": "object",
        "properties": {
          "event": { "const": "start_subscription" },
          "settings": { "$ref": "#/$defs/Settings" }
        },
        "additionalProperties": false,
        "required": ["event"]
      },
      {
        "type": "object",
        "properties": {
          "event": { "const": "stop_subscription" }
        },
        "additionalProperties": false,
        "required": ["event"]
      }
    ]
  }
}
```

## Property Design

  - Clearly separate:
    - `topicPayload`: input/request data
    - `data`: output/response data

  - All objects must:
    - Define `properties`
    - Set `additionalProperties: false`
    - Explicitly declare `required` fields

  - Prefer **compact syntax** for simple property definitions (e.g., only `type`)

```json
{
  // Do:
  "properties": {
      "someRequiredProperty": { "type": "number" }
  },
  // Don't do:
  "properties": {
    "someRequiredProperty": {
        "type": "number"
    }
  }
}
```

  - Use **expanded multi-line syntax** when a property has multiple constraints (e.g., `enum`, `description`, `array` etc.)

```json
{
  // Do:
  "properties": {
    "event": {
      "type": "string",
      "enum": ["start_subscription", "stop_subscription"]
    }
  },
  // Don't do:
  "properties": {
    "event": { "type": "string", "enum": ["start_subscription", "stop_subscription"] }
  },
  // Do:
  "deltaTimeSteps": {
    "type": "array",
    "items": { "type": "number" }
  },
  // Don't do:
  "deltaTimeSteps": { "type": "array", "items": { "type": "number" } }
}
```

  - Use `{ "const": "value" }` for fields that always have a single known value. When reviewing C++ source, look for `constexpr std::string_view` literals that flow directly into JSON output - these should always be typed as `const` rather than `string`:

```json
{
  // Do - when the C++ always sets this to "Keybindings":
  "name": { "const": "Keybindings" },
  // Don't do:
  "name": { "type": "string" }
}
```

  - `$ref` should always be written compactly on a single line unless it has sibling keywords:

```json
{
  // Do:
  "level": { "$ref": "#/$defs/LogLevel" },
  // Do (with sibling):
  "type": {
    "$ref": "#/$defs/SomeType",
    "description": "The type of event"
  }
}
```

## Named Union Types in $defs

When a `topicPayload` or `data` has multiple distinct shapes (e.g. different commands or response types), define each shape as a named `$defs` entry and collect them into a union type. This produces clean, navigable TypeScript output:

```json
{
  "$defs": {
    "ConnectCommand": {
      "type": "object",
      "properties": {
        "type": { "const": "connect" }
      },
      "additionalProperties": false,
      "required": ["type"]
    },
    "DisconnectCommand": {
      "type": "object",
      "properties": {
        "type": { "const": "disconnect" }
      },
      "additionalProperties": false,
      "required": ["type"]
    },
    "TopicCommand": {
      "anyOf": [
        { "$ref": "#/$defs/ConnectCommand" },
        { "$ref": "#/$defs/DisconnectCommand" }
      ]
    }
  }
}
```

This generates the TypeScript equivalent of:
```ts
export type TopicCommand = ConnectCommand | DisconnectCommand;
```

## Integer Enums and tsEnumNames

When a field is serialized as an integer from a C++ enum, use `tsEnumNames` to produce a named TypeScript enum rather than a bare numeric union. This is a `json-schema-to-typescript` specific extension:

```json
{
  "$defs": {
    "LogLevel": {
      "type": "integer",
      "enum": [0, 1, 2, 3, 4, 5, 6, 7],
      "tsEnumNames": ["AllLogging", "Trace", "Debug", "Info", "Warning"]
    }
  }
}
```

This generates:
```ts
export enum LogLevel {
  AllLogging = 0,
  Trace = 1,
  Debug = 2,
  ...
}
```

Note that if the same enum is used across multiple topics it is a strong candidate for a shared global schema file.

## Shared / Global Definitions

Some types are used across multiple topics and should be extracted into shared helper schema files rather than redefined per topic. Rather than a single monolithic file, types are organized into focused helper files by domain. `json-schema-to-typescript` resolves cross-file `$ref` pointers via its built-in `$RefParser`, so shared types are fully supported.

### Helper Schema Files

Each helper file lives alongside the topic schemas in `OpenSpace/support/types/` and contains only `$defs` - no top-level `title` or topic contract is needed.

Current helper files and their intended contents:

Helper schema files should be created as needed when types are shared across multiple topics. Reference them using relative paths in `$ref`.

| File | Contents |
| --- | --- |
| `properties.json` | `AnyProperty`, `PropertyOwner`, `BaseMetaData`, all property type defs, `AdditionalData*`, `ViewOptions` |
| `scenegraph.json` | `SceneGraphNode`, `Vec3` |
| `logging.json` | `LogLevel`, `LogLevelString` |
| `actions.json` | `Action`, `Keybind` |
| `events.json` | `EventType`, `EventData` and all per-event data defs |

A helper file looks like this:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$defs": {
    "LogLevel": {
      "type": "integer",
      "enum": [0, 1, 2, 3, 4, 5, 6, 7],
      "tsEnumNames": [
        "AllLogging", "Trace", "Debug", "Info",
        "Warning", "Error", "Fatal", "NoLogging"
      ]
    },
    "LogLevelString": {
      "type": "string",
      "enum": ["All", "Trace", "Debug", "Info", "Warning", "Error", "Fatal", "None"]
    }
  }
}
```

Reference a shared type from any topic schema using a relative file path:

```json
{
  "properties": {
    "level": { "$ref": "logging.json#/$defs/LogLevel" },
    "node": { "$ref": "scenegraph.json#/$defs/SceneGraphNode" }
  }
}
```

TypeScript type generation is handled by `generatetopictypes.mjs` in the [JavaScript repository](https://github.com/OpenSpace/openspace-api-js). The script uses `compileFromFile` which automatically resolves relative `$ref` paths using the schema file's directory as the base, so cross-file refs work without any script changes. Helper files are excluded from individual topic compilation since the script filters for files ending in `topic.json`.

To generate standalone `.d.ts` files for the helper schemas, add a separate npm script per file using `--unreachableDefinitions` so all `$defs` are emitted even if not referenced at the top level:

```sh
json2ts -i support/types/logging.json -o src/types/generated/logging.d.ts \ --unreachableDefinitions
```

### Flagging Candidates for Extraction

When a type in a topic schema is identified as a candidate for extraction, add a
`description` note so it can be tracked:

```json
{
  "$defs": {
    "LogLevel": {
      "description": "TODO: extract to logging.json - also used in errorlogtopic",
      "type": "integer"
    }
  }
}
```

## C++ Source as Source of Truth

The C++ source is always the source of truth for the schema. When reviewing or writing a schema:

  - Verify field names match exactly - including casing (e.g. `camelCase` vs `snake_case`)
  - Trace `constexpr std::string_view` literals and similar constants that flow directly into JSON keys or values - these must be typed as `const` rather than `string`
  - Copy field names and values verbatim from the C++ source to the schema
  - Flag any inconsistencies between the schema and the source as bugs to be fixed in the C++, not worked around in the schema
  - Check optional fields - only fields guarded by `has_value()`, `find()`, or `value_or` with a meaningful fallback should be omitted from `required`
  - Watch for and report misspellings in C++ identifiers, string literals, and comments - these indicate bugs to fix, not schema workarounds

## Example Base JSON Schema

Below is a minimal base schema that all topic schemas must adhere to:

```json
{
  "$defs": {
    "SomeType": {
      "type": "string",
      "enum": ["One", "Two"]
    }
  },
  "title": "TopicName",
  "type": "object",
  "properties": {
    "topicId": { "const": "TopicIdentifier" },
    "topicPayload": {
      "type": "object",
      "properties": {
        "someRequiredProperty": { "type": "number" },
        "someType": { "$ref": "#/$defs/SomeType" }
      },
      "additionalProperties": false,
      "required": ["someRequiredProperty"]
    },
    "data": {
      "type": "object",
      "properties": {},
      "required": [],
      "additionalProperties": false
    }
  },
  "additionalProperties": false,
  "required": ["topicId", "topicPayload", "data"]
}
```

## Review Checklist

Before submitting a schema, ensure the following:

  - Top-level contains only: `topicId`, `topicPayload`, `data`
  - `additionalProperties: false` is set on all objects
  - `topicId.const` matches `server.cpp`
  - All required fields are explicitly listed
  - No unused `$defs`
  - `enum` is used instead of `anyOf` where possible
  - `const` used for all fields with a single known fixed value from C++ source
  - Integer enums use `tsEnumNames` to produce named TypeScript enums
  - Named union types defined in `$defs` for multi-shape `topicPayload` and `data`
  - Shared types flagged for extraction into a global schema file
  - C++ source reviewed for misspellings and inconsistencies
