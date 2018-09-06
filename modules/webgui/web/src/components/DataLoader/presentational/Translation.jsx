import React from 'react';
import PropTypes from 'prop-types';
import 'react-select/dist/react-select.css';
import Label from '../../common/Label/Label';
import Select from '../../common/Input/Select/Select';
import styles from './Translation.scss';

const Translation = (props) => {
  const { onSetTranslationTarget, target } = props;
  const spiceOptions = 'SUN EARTH'.split(' ').map(v => ({ value: v, label: v }));
  return (
    <div className={styles.wrapper}>
      <Label size='medium'>Apply Spice Translation</Label>
      <Select label={target}
        options={spiceOptions}
        onChange={onSetTranslationTarget}
        placeholder=''
      />
    </div>
  );
}

Translation.propTypes = {
  onSetTranslationTarget: PropTypes.func.isRequired,
  target: PropTypes.string.isRequired
};

Translation.defaultProps = {
  onTranslationTypeChange: () => { },
  onSetTranslationTarget: () => { },
  target: 'Sun',
};

export default Translation;