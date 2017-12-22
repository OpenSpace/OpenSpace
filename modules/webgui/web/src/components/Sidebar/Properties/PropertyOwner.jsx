import React from 'react';
import PropTypes from 'prop-types';
import ToggleContent from '../../common/ToggleContent/ToggleContent';
import Property from './Property';
import BoolProperty from './BoolProperty';
import NumericProperty from './NumericProperty';
import OptionProperty from './OptionProperty';
import TriggerProperty from './TriggerProperty';
import VecProperty from './VectorProperty';
import MatrixProperty from './MatrixProperty';
import TransferFunctionEditor from './TransferFunctionEditor'

const types = {
  BoolProperty,
  OptionProperty,
  TriggerProperty,
  StringProperty: Property,
  NumericProperty,
  FloatProperty: NumericProperty,
  IntProperty: NumericProperty,
  Vec2Property: VecProperty,
  Vec3Property: VecProperty,
  Vec4Property: VecProperty,
  MatrixProperty,
  DMat4Property: MatrixProperty,
  defaultProperty: Property,
  TransferFunctionHandler: TransferFunctionEditor,
};

const PropertyOwner = ({ name, properties, subowners }) => (
  <ToggleContent title={name}>
    { subowners.map(subowner => {
      if (subowner.name in types) {
        var ReturnType = types[subowner.name];
        return < ReturnType {...subowner} key={subowner.name} />
      }
      else
        return <PropertyOwner {...subowner} key={subowner.name} />
    }) }
    { properties.map((prop) => {
      const { Description } = prop;
      const Type = types[Description.Type] || types.defaultProperty;
      return (
        <Type key={Description.Identifier} {...prop} subscribe />
      );
    }) }
  </ToggleContent>
);

PropertyOwner.propTypes = {
  name: PropTypes.string.isRequired,
  properties: PropTypes.arrayOf(PropTypes.object),
  subowners: PropTypes.arrayOf(PropTypes.shape({
    name: PropTypes.string,
    subowners: PropTypes.array,
    properties: PropTypes.array,
  })),
};

PropertyOwner.defaultProps = {
  properties: [],
  subowners: [],
};

export default PropertyOwner;
export const Types = types;
export const GetType = type => types[type] || types.defaultProperty;
