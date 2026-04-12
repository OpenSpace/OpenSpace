# Writing a JSON Schema

Schemas should preferably be written directly as JSON files to ensure correct syntax highlighting, formatting, and validation.

LLMs can be useful for generating an initial draft, but the result must always be reviewed and aligned with existing conventions. When using an LLM, provide:

  - One or two examples of existing schemas
  - Relevant engine source code for the topic

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

All schemas **must** define exactly the following top-level properties:

  - `topicId`
  - `topicPayload`
  - `data`

No additional top-level properties are allowed. This must be enforced using:

```json
"additionalProperties": false
```

at the root level.

### topicId

`topicId` is the unique identifier of the topic, this must match the identifier defined in `server.cpp`. This identifier is used by the API to select which topic to start.

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
    - Must be set to `false` unless there is a strong, well-justified reason not to
    - Applies to all objects unless explicitly required otherwise
  - Enums vs unions
    - Use `enum` for simple sets of constant values (e.g., strings or numbers)
    - Avoid `anyOf` when `enum` is sufficient
  - `anyOf` vs `oneOf`
    - Prefer `anyOf` over `oneOf` for performance and predictability
    - Only use `oneOf` when strict mutual exclusivity is required
    - See [documentation](https://json-schema.org/blog/posts/applicability-json-schema-fundamentals-part-1#putting-everything-together-avoiding-oneof-pitfalls)

## Property Design

  - Clearly separate:
    - `topicPayload`: input/request data
    - `data`: output/response data

  - All objects must:
    - Define `properties`
    - Set `additionalProperties: false`
    - Explicitly declare `required` fields

  - Prefer compact syntax for properties and their types if they are simple i.e., `string`, `number`, `boolean` etc. If a property has multiple fields that defines it, each key gets its own line.

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

Before submitting a schema, verify:

  - Top-level contains only: `topicId`, `topicPayload`, `data`
  - `additionalProperties: false` is set on all objects
  - `topicId.const` matches `server.cpp`
  - All required fields are explicitly listed
  - No unused `$defs`
  - `enum` is used instead of `anyOf` where possible
